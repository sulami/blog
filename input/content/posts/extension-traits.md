title = "Extension Traits"
slug = "extension-traits"
created_at ="2025-03-03"
tags = ["rust", "espresso"]
---
In Rust, it can sometimes be convenient to add a method to a preexisting type.

Rust allows us to implement a trait for a type as long as we own either the
trait or the type. This means we cannot implement
[`Iterator`](https://doc.rust-lang.org/stable/core/iter/trait.Iterator.html) for
[`u32`](https://doc.rust-lang.org/stable/core/primitive.u32.html), but we can
implement `Iterator` for `MyType`, and we can implement `MyTrait` for `u32`.[^1]
The former is very common, either through the
[`derive`](https://doc.rust-lang.org/stable/reference/attributes/derive.html)
macro like `#[derive(Copy, Clone, Debug)]`, or explicitly like when we implement
[`Display`](https://doc.rust-lang.org/std/fmt/trait.Display.html) for a type.

The latter is what we call extension traits, a tool we can use to add methods to
existing types. The idea of extension traits is to sidestep the fact that we
cannot add methods via `impl` blocks directly to types we do not own by instead
adding a trait, implementing methods for the trait, and implementing the trait
for the type we do not own. In this roundabout way we can extend a type's
interface, but crucially only for users who opt in by importing the extension
trait, avoiding a situation [like the one in
Ruby](https://github.com/garybernhardt/base). This, for example, is how the
popular [itertools](https://crates.io/crates/itertools) crate works, through the
[`Itertools`](https://docs.rs/itertools/latest/itertools/trait.Itertools.html)
extension trait.

## An Example

We can already reverse
[`Vec<T>`s](https://doc.rust-lang.org/stable/alloc/vec/struct.Vec.html), but we
cannot reverse
[`String`s](https://doc.rust-lang.org/stable/alloc/string/struct.String.html).
Would it not be nice to add that functionality?[^2] This is the interface we
will build, analogous to how `Vec` works:

```rust
let mut s = String::from("foo");
s.reverse();
assert_eq!(s, "oof");
```

First, we need a new extension trait:

```rust
pub trait ReversibleString {
    fn reverse(&mut self);
}
```

We then implement the trait for `String`:[^3]

```rust
impl ReversibleString for String {
    fn reverse(&mut self) {
        *self = self.chars().rev().collect();
    }
}
```

And that is already all we need. As long as the trait is in scope, we can
use `reverse` on `String`s.

[^1]: You also can't get around this through an intermediate trait and a blanket
    implementation, I tried.

[^2]: Whether we should or not is a topic for another day, we are just going to
    do it. There's a whole thing around characters vs. bytes, variable width
    unicode characters, etc.

[^3]: I know, I know, can't just reverse the characters, it'll break Unicode.
    Listen, it's an example.
