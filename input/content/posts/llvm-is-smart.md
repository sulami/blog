title = "LLVM is Smarter Than Me"
timestamp = "2024-04-19"
tags = ["rust", "compilers", "espresso"]
---

<script src="/mathjax-3.2.2/tex-chtml.js" async></script>

I was reading [Algorithms for Modern Hardware](https://en.algorithmica.org/hpc/)
recently, specifically [the chapter on
SIMD](https://en.algorithmica.org/hpc/simd/), and was impressed by
auto-vectorization. The basic idea is that modern CPUs have so-called SIMD[^1]
instructions that allow applying an operation to several values at once, which
is much faster than performing the equivalent scalar instructions one at a time.
Modern compilers can detect certain programming patterns where an operation is
performed repeatedly on scalar values and group the operations to make use of
the faster instructions.[^2]

The book uses primarily C++ for its examples, but I was curious if the same
pattern would work in Rust as well, without having to manually annotate
anything. So I opened up the [compiler explorer](https://godbolt.org/) and typed
in my first test, which looked like this:

```rust
#[no_mangle]
fn sum() -> u32 {
    (0..1000).sum()
}
```

To make sure the compiler would feel safe to use SIMD instructions, I used `-C
opt-level=3 -C target-cpu=skylake`, telling it that the target CPU supports
them. But instead of SIMD instructions I got this assembly:

```asm
sum:
        mov     eax, 499500
        ret
```

Turns out I was outsmarted by the compiler, which used [constant
folding](https://en.wikipedia.org/wiki/Constant_folding) to directly return the
final value it computed at compile time. To avoid this optimization from
happening we can pass an argument into the function, so that the compiler cannot
know the final result:

```rust
#[no_mangle]
fn sum(n: u32) -> u32 {
    (0..n).sum()
}
```

Which generates this assembly:

```asm
sum:
        test    edi, edi
        je      .LBB0_1
        lea     eax, [rdi - 1]
        lea     ecx, [rdi - 2]
        imul    rcx, rax
        shr     rcx
        lea     eax, [rdi + rcx]
        dec     eax
        ret
.LBB0_1:
        xor     eax, eax
        ret
```

Still there are no SIMD instructions in here, which would usually start with the
letter `v`, such as
[`vpaddd`](https://www.felixcloutier.com/x86/paddb:paddw:paddd:paddq) which adds
integers in parallel. To compare, I wrote the equivalent program in C++:

```c++
unsigned int sum(unsigned int n) {
    unsigned int rv = 0;
    for (unsigned int i = 0; i < n; i++) {
        rv += i;
    }
    return rv;
}
```

Compiler explorer defaults to GCC to compile C++. I used the equivalent `-O3
-march=skylake`, and sure enough, I got back 106 lines of assembly (omitted for
brevity), including the desired SIMD instructions. I then switched to Clang, the
LLVM-based C/C++ compiler that is also used by Rust, and it produced the exact
same assembly as it did for the Rust version. This tells us the difference is
not one of languages, but one of compilers.[^3]

A quick comparative benchmark revealed the LLVM version to be significantly
faster than GCC's vectorized loop, especially for large values of `n`, which is
unsurprising when looking at the instructions, about 10 scalar instructions will
beat hundreds of loops over vector instructions.

The key here is that the sum of consecutive integers from zero to n has a
closed form solution, which I only realized after looking more closely at the
assembly:

$$\sum^n_{i=0}{i} = \frac{n (n+1)}{2}$$

LLVM apparently detects that that is exactly what we are trying to do, and as a
result it can do away with the looping altogether and directly calculate the
result in one step, changing `sum` from O(n) to O(1). Colour me impressed.

Finally, replicating the book's example more closely, I managed to get LLVM to
automatically vectorize a loop for me, using the following code:

```rust
#[no_mangle]
fn sum(arr: &[u32]) -> u32 {
    arr.iter().sum()
}
```

This works because LLVM has no idea about the contents of the array, so it
cannot deduce a more efficient way of calculating their sum than to actually add
them all up.

My takeaway here is that LLVM is even better at generating optimal code than I
would have expected, at least for trivial examples. I am happy that I can
continue writing idiomatic code without feeling like I am trading off
performance for readability.

[^1]: Single Instruction Multiple Data

[^2]: There are some details around having to either pad the last group so it
    lines up with the group size or run some scalar instructions at the end,
    it's really quite clever.

[^3]: Interestingly enough, the LLVM IR generated is quite different between the
    two languages.
