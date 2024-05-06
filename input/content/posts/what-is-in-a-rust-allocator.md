title = "What is in a Rust Allocator?"
slug = "what-is-in-a-rust-allocator"
timestamp = "2024-05-06"
tags = ["rust"]
---

Recently I was reading the documentation for
[jemalloc](https://crates.io/crates/jemallocator) and wondered what actually
goes into a custom memory allocator for Rust. To add jemalloc, or any other
custom allocator, to a Rust application, one uses the
[`#[global_allocator]`](https://doc.rust-lang.org/std/prelude/v1/attr.global_allocator.html)
macro. Conveniently, [the documentation for
`GlobalAlloc`](https://doc.rust-lang.org/std/alloc/trait.GlobalAlloc.html)
actually has a full example allocator for us to explore. It starts out as
follows:

```rust
const ARENA_SIZE: usize = 128 * 1024;
const MAX_SUPPORTED_ALIGN: usize = 4096;
#[repr(C, align(4096))] // 4096 == MAX_SUPPORTED_ALIGN
struct SimpleAllocator {
    arena: UnsafeCell<[u8; ARENA_SIZE]>,
    remaining: AtomicUsize, // we allocate from the top, counting down
}
```

We begin by defining a constant
[arena](https://en.wikipedia.org/wiki/Region-based_memory_management) size of
128 kB, which will be statically allocated at startup. Arenas are often faster
because they make fewer memory allocation requests to the operating system and
also reduce memory fragmentation, at the cost of memory overhead due to the
unused space in the arena. In almost every real world application, a memory
allocator would use many smaller arenas as opposed to one large one, and also
have the capabilities to add additional arenas at runtime as the existing ones
fill up.

Next we define the representation via `repr`. The representation describes the
layout of this type in memory. The default representation is `Rust`, which means
the Rust compiler can do whatever it wants for optimization purposes. `repr(C)`
lays out the memory like C/C++ do, which is useful for interoperability with
other languages. It is also somewhat more predictable, as those layouts do not
change anymore. [The Rust
reference](https://doc.rust-lang.org/reference/type-layout.html) has a detailed
breakdown of the different layout options.

In this case we also define a memory alignment, which means we are saying that
the memory address of the allocator should be a multiple of 4096. Rust will also
make sure that each of the fields will be aligned in this way, and add padding
in between the fields if necessary. We do this because clients will later
request memory with a certain memory alignment themselves, and aligning the
overall array means any aligned offset inside the array will also be aligned
automatically.

The struct itself has two fields. The arena is a fixed size byte array, wrapped
in an [`UnsafeCell`](https://doc.rust-lang.org/std/cell/struct.UnsafeCell.html).
`UnsafeCell` is the base for all the regular, safe cells like
[`Cell`](https://doc.rust-lang.org/std/cell/struct.Cell.html) or
[`RefCell`](https://doc.rust-lang.org/std/cell/struct.RefCell.html), all of
which enable interior mutability. This is required because
[`GlobalAlloc`](https://doc.rust-lang.org/std/alloc/trait.GlobalAlloc.html)'s
`alloc` method takes a shared `&self` reference, not a mutable `&mut self` one,
and returns a `*mut u8` pointer into the interior array. `UnsafeCell` gives us
the required escape hatch to return a mutable pointer into a struct we only have
a shared reference to.

The second field `remaining` is an offset from the start of the arena,
effectively acting as a pointer into the arena. Because we are counting down
"from the top," it starts out with the size of the arena, going down as memory
is allocated.The benefit of this approach is that checking if we have enough
memory available to satisfy a request is as simple as checking if `remaining` is
at least as large as the requested amount of memory.[^1]

`remaining` is an
[`AtomicUsize`](https://doc.rust-lang.org/std/sync/atomic/struct.AtomicUsize.html).
Atomics are used to signal to both the compiler and the CPU that ordering of
memory access is important to ensure correctness. If we were using a plain
`usize`, the compiler or the CPU could decide that it would be faster to reorder
reads or writes, compromising correctness. Rust inherits the C++ atomics memory
model, which comes with a few ways of describing access restrictions, which we
will dig into in a bit. The Rustonomicon also has [a whole
section](https://doc.rust-lang.org/nomicon/atomics.html) on atomics with more
details.[^2]

```rust
#[global_allocator]
static ALLOCATOR: SimpleAllocator = SimpleAllocator {
    arena: UnsafeCell::new([0x55; ARENA_SIZE]),
    remaining: AtomicUsize::new(ARENA_SIZE),
};
```

This is the bit that actually selects the globally used allocator, using the
aforementioned
[`#[global_allocator]`](https://doc.rust-lang.org/std/prelude/v1/attr.global_allocator.html)
macro. It is simply creating a static instance of the allocator, initializing
the arena with `0x55`.[^3] 

```rust
unsafe impl Sync for SimpleAllocator {}
```

We then implement the [`Sync` marker
trait](https://doc.rust-lang.org/std/marker/trait.Sync.html) for our allocator,
which does not have any methods, but simply tells the compiler that it is safe
to share references to the allocator across thread boundaries. This is an unsafe
trait, and it requires us to make sure that the resulting behaviour is actually
sound. We will see below how we actually accomplish this.

Now let us look at the implementation for
[`GlobalAlloc`](https://doc.rust-lang.org/std/alloc/trait.GlobalAlloc.html),
again an unsafe trait.

```rust
unsafe impl GlobalAlloc for SimpleAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let size = layout.size();
        let align = layout.align();
```

The first required method is
[`alloc`](https://doc.rust-lang.org/std/alloc/trait.GlobalAlloc.html#tymethod.alloc),
which requests some memory from the allocator according to
[`Layout`](https://doc.rust-lang.org/std/alloc/struct.Layout.html), expecting a
pointer to that memory. If there is no memory matching the request available, we
return a null pointer.
[`Layout`](https://doc.rust-lang.org/std/alloc/struct.Layout.html) has two
methods that are interesting to us,
[`size`](https://doc.rust-lang.org/std/alloc/struct.Layout.html#method.size) and
[`align`](https://doc.rust-lang.org/std/alloc/struct.Layout.html#method.align),
which return the desired size and alignment respectively. The size is just a
number of bytes, and the alignment works just the same as we noted above when
looking at `repr`.

```rust
        // `Layout` contract forbids making a `Layout` with align=0, or align not power of 2.
        // So we can safely use a mask to ensure alignment without worrying about UB.
        let align_mask_to_round_down = !(align - 1);
```

This comment mentions a contract, a requirement that is not captured outside of
documentation. We can assume that alignment is a non-zero power of two. This is
important for the next step. We set up a bitmask that we will use for ensuring
the correct alignment.

Alignment is a power of two by contract, which means it's a single set bit. By
decrementing it by one, we transform e.g. 8 (`00001000`) to 7 (`00000111`), and
then invert that to set all high bits up to and including the original one,
`11111000`. We will then `&` this onto our memory address later, which acts as a
round down by setting all the lower bits to zero. Rounding down works because we
made sure to align the overall array, and are allocating from the top down, so
that rounding down introduces a gap "above" the newly allocated memory. If we
were counting from the bottom up, we would be rounding up instead to introduce a
gap "below."

```rust
        if align > MAX_SUPPORTED_ALIGN {
            return null_mut();
        }
```

At this point we just check if the client requested an alignment higher than we
have chosen to support, in which case we return a null pointer to signal that we
cannot fulfill the request. Again, this is dependent on the alignment of the
arena array.

```rust
        let mut allocated = 0;
        if self
            .remaining
            .fetch_update(SeqCst, SeqCst, |mut remaining| {
                if size > remaining {
                    return None;
                }
                remaining -= size;
                remaining &= align_mask_to_round_down;
                allocated = remaining;
                Some(remaining)
            })
            .is_err()
        {
            return null_mut();
        };
```

This is where the heavy lifting of allocation is done, and there is a lot to
unpack here. We recall that `remaining` is an
[`AtomicUsize`](https://doc.rust-lang.org/std/sync/atomic/struct.AtomicUsize.html)
describing an offset from the start of the arena, counting down as we allocate
memory. Atomics rely on the underlying platform to maintain the correct order of
operations, which is why they cannot be read or set like regular variables, but
have a special set of methods to access them. In this case we are using the
[`fetch_update`
](https://doc.rust-lang.org/std/sync/atomic/struct.AtomicUsize.html#method.fetch_update)
method, which is a form of
[read-write-modify](https://en.wikipedia.org/wiki/Read%E2%80%93modify%E2%80%93write)
instruction. As mentioned above, if we were not using an atomic, either the
compiler or the CPU itself could decide to reorder the different reads and
writes to the counter for performance optimizations, compromising correctness.

`remaining` starts out pointing at the start of the previous allocation, if any,
so we just subtract the size of the new allocation. Note that this is also where
we use our bitmask to round down the address to get the right alignment.

The function passed to the
[`fetch_update`](https://doc.rust-lang.org/std/sync/atomic/struct.AtomicUsize.html#method.fetch_update)
method returns `Some(new_value)`, in this case `Some(remaining)`, but
`fetch_update` itself returns `Result<old_value, Err>`. Incidentally the passed
function might also be called multiple times if there is contention from other
threads. The mutable variable `allocated` in the outer scope allows us to
extract the new value of `remaining` without re-fetching `remaining` and risking
a data race.

`fetch_update` also takes two arguments which in this case are both `SeqCst`,
short for _sequentially consistent_. These are atomic memory orderings inherited
from C++'s memory model, and specify the level of synchronization required when
accessing this memory. There are a few different levels of which this is the
strictest, guaranteeing that no reads and writes are reordered such that they
switch order with the access to `remaining`. This is important to avoid data
races that could lead to overlapping allocations or over-committing memory that
is not actually available.

```rust
        self.arena.get().cast::<u8>().add(allocated)
    }
```

We get a pointer to the start of the arena, add the offset of the newly
allocated region from the start of the arena, and then return it as a pointer to
that same region.

This is the reason for wrapping the arena in an
[`UnsafeCell`](https://doc.rust-lang.org/std/cell/struct.UnsafeCell.html), it
allows us to get a mutable pointer to its contents from a shared reference. We
cannot use
[`std::ptr::addr_of_mut!`](https://doc.rust-lang.org/std/ptr/macro.addr_of_mut.html)
or just coerce a reference to the right type because we only have a shared
reference to `self`, and thus cannot derive a mutable reference without interior
mutability. We could use a
[`RefCell`](https://doc.rust-lang.org/std/cell/struct.RefCell.html) and its
[`as_ptr`](https://doc.rust-lang.org/std/cell/struct.RefCell.html#method.as_ptr)
method though.[^4]

```rust
    unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {}
```

Releasing memory in this case is a no-op, which means that memory once allocated
is gone forever, even after the contained values have been dropped. In a
real-world application, we would probably want to keep track of memory that has
become available again, so that we could use it again for new allocations.

And that is all that is to it. Of course this is a very simple allocator, but it
is actually functional. It does not actually dynamically request memory from the
operating system, but that is actually a common pattern in embedded development
where there is no operating system. Still, in those scenarios we would probably
want to reuse freed memory, which would require keeping track of which regions
in the arena are currently in use and which are not, if we can live with the
memory overhead required to store that information.

[^1]:  A potential downside is that we cannot simply grow the most recent
    allocation without moving it, as would be possible if counting up from zero.

[^2]: For those curious, [Memory Barriers: a Hardware View for Software
    Hackers](http://www.rdrop.com/users/paulmck/scalability/paper/whymb.2009.04.05a.pdf)
    has a low level description of why and how ordering is done inside CPUs. 

[^3]: To the best of my knowledge this value is selected because it results in
    alternating ones and zeroes, which might aid in debugging.

[^4]: I actually don't know why the author chose not to. [The pull request
    adding this example](https://github.com/rust-lang/rust/pull/81864) has no
    discussion about this option.
