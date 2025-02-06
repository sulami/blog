title = "String vs &str"
slug = "string-vs-str"
tags = ["rust", "software-design"]
timestamp = "2025-02-06"

---

Something I see somewhat regularly when engineers adopt Rust, almost regardless
of their level of experience, is the confusion between
[`&str`](https://doc.rust-lang.org/stable/core/primitive.str.html) and
[`String`](https://doc.rust-lang.org/stable/alloc/string/struct.String.html). To
add to the confusion, both are used in different situations, one of them can
transparently convert to the other under the correct conditions, and there is
generally an optimal choice to be made.

An important fact to notice is that `&str` is a reference, which means it does
not own the memory holding the string, it is merely a pointer to a string
elsewhere. This means it has a known size[^1] and can thus be stored on the
stack instead of on the heap, but limits where it can be passed. It can only be
alive as long as its source also is alive.

It is worth mentioning that most of this also holds for
[`Vec`](https://doc.rust-lang.org/stable/alloc/vec/struct.Vec.html) vs.
[`slice`](https://doc.rust-lang.org/stable/core/primitive.slice.html).

[^1]: The same as a
    [`usize`](https://doc.rust-lang.org/stable/core/primitive.usize.html).

## Function Arguments

If you are writing a function, it depends on whether you need a `String` within
your function, or if a `&str` will do. In general, we want to be as permissive as
possible when accepting arguments, which means accepting a `&str`.

```rust
fn show(s: &str) {
    println!("{s}");
}

// Both work.
show("foo");
show(&String::from("foo"));
```

This works because `String` has an implementation for
[`Borrow<str>`](https://doc.rust-lang.org/stable/alloc/borrow/trait.Borrow.html),
which means it can be borrowed into a `&str` that is just a pointer to its
internal string slice. This in turn implies that the `&str` generated from
borrowing is only alive as long as the `String` is alive, as discussed above.

Should the function require a `String`, for example to insert it into a struct,
the best solution is to accept an [`impl
Into<String>`](https://doc.rust-lang.org/stable/core/convert/trait.Into.html).

```rust
struct Foo {
    s: String,
}

impl Foo {
    fn new(s: impl Into<String>) -> Self {
        Self {
            s: s.into(),
        }
    }
}

// Both work.
Foo::new("foo");
Foo::new(String::from("foo"));
```

This interface allows the caller to pass either type, and if they pass a
`String` the `.into()` gets optimized away into a no-op. In comparison,
accepting a `&str` and constructing a `String` within the function would be
similarly permissive, but wasteful if the caller already had a `String`.

## Function Return Values

When returning a string from a function, the decision depends a bit more on
context. Return a `&str` in one of two situations:

1. You are returning a static string that you don't expect to be mutated later
   on, such as an error or log message.

```rust
fn foo() -> &'static str {
   "foo"
}
```

2. You are extracting the returned string from one of the function arguments,
   such as a substring matching some precondition. In this case it is usually
   prudent to not pay the cost to allocate a new `String` if the caller might
   not need it, and leave it up to the caller instead.

```rust
fn start(s: &str) -> &str {
   &s[..5]
}
```

Otherwise, return a `String`. The compiler will often force you to do so when
appropriate.

## Struct Fields

Struct fields mostly match the second rule of function return values,[^2] embed
a `&str` in a struct only if the struct will relate to the origin of the `&str`.
Again, thinking about finding substrings, one could imagine this struct:

```rust
struct SubstringMatch<'a> {
    /// The location of the match.
    start: usize,
    /// The matching substring.
    text: &'a str,
}
```

The compiler will force you to spell out the lifetimes, commonly `<'a>` by
convention, which in this case dictates that the entire `SubstringMatch` struct
can only be alive as long as the `&str` in the `text` field is, otherwise we
would have a pointer pointing to unknown memory. This means `SubstringMatch` is
only useful in contexts where the entire original string is also alive in, which
is not an unreasonable assumption. Incidentally it also means the original
string cannot be mutated while a `SubstringMatch` is holding a reference to it.
In return we gain some efficiency by embedding a pointer into the original
string instead of constructing a new `String` every time, using up memory to
store duplicate strings and paying the performance penalty for extra
allocations.

[^2]: Embedding static strings in a struct is possible, but I have yet to see a
    use case for it.
