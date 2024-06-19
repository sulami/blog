title = "Demystifying Rust's ? Operator"
slug = "demystifying-rusts-questionmark-operator"
timestamp = "2024-06-19"
tags = ["rust"]
---

When talking to folks new to the Rust language, a common misconception I
encounter is that [`?`][qmark] is some kind of special syntax that only works
for [`Result`][result]. While it is true that the `?` operator is a special bit
of syntax, it is not limited to [`Result`][result], and a lesser known fact is
that it works for [`Option`][option] as well:

```rust
fn maybe_double(v: Option<u32>) -> Option<u32> {
    Some(v? * 2)
}
```

In fact, `?` is a generic short-circuit operator that can be used for arbitrary
types, as long as they implement the [`Try`][try] trait. To demonstrate, let us
implement a new type that does so. Imagine we want to have a type that is both
optional and fallible, some kind of `OptionResult`:[^1]

[^1]: Of course you wouldn't go through the hassle of doing this, you'd just
use a `Result<Option<T>, E>`.

```rust
enum OptionResult<T, E> {
    Some(T),
    None,
    Err(E),
}
```

This type can be some value of type `T`, nothing, or an error of type `E`. 

To be able to use the [`Try`][try] trait, we need to use the nightly compiler at 
the moment, as it is not yet stabilized. We then also need to activate the 
feature for the trait, and import a few dependencies:

```rust
#![feature(try_trait_v2)]

use std::ops::{ControlFlow, FromResidual, Try};
```

[`Try`][try] requires implementing types to also
implement [`FromResidual`][from-residual]. A
residual is the type returned on the short-circuit path, the error
for [`Result`][result],
or nothing for [`Option`][option]. The implementation
for [`FromResidual`][from-residual]
is quite simple, all we have to do is wrap the value:

```rust
impl<T, E> FromResidual for OptionResult<T, E> {
    fn from_residual(residual: <Self as Try>::Residual) -> Self {
        Self::Err(residual)
    }
}
```

Next we already get to implementing [`Try`][try], which is the last piece of the
puzzle. Here we have a decision to make. [`Try`][try] has a method `branch`,
which returns a [`ControlFlow`][control-flow]. [`ControlFlow`][control-flow] is
an enum that decides whether we should continue execution with an unwrapped
output, or short-circuit and return a residual. For the sake of argument, we
will implement `OptionResult` such that its `Some` and `None` arms continue
execution and unwrap into a regular [`Option`][option], and `Err` will
short-circuit just like [`Result`][result] does. In practice this will mean:

```rust
fn some_function() -> OptionResult<u32, &'static str> {
    // This continues.
    let output: Option<u32> = OptionResult::Some(42)?;
    
    // This continues.
    let output: Option<u32> = OptionResult::None?;
    
    // This short-circuits.
    let output: Option<u32> = OptionResult::Err("oh no")?;
}
```

Here is the corresponding implementation for [`Try`][try]:

```rust
impl<T, E> Try for OptionResult<T, E> {
    type Output = Option<T>;
    type Residual = E;

    fn from_output(output: Self::Output) -> Self {
        match output {
            Some(t) => Self::Some(t),
            None => Self::None,
        }
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            Self::Some(t) => ControlFlow::Continue(Some(t)),
            Self::None => ControlFlow::Continue(None),
            Self::Err(e) => ControlFlow::Break(e),
        }
    }
}
```

We can see the definition of two associated types, `Output` is what we 
unwrap to on the continuation path, and `Residual` is the aforementioned 
short-circuit type. We also have to implement `from_output`, which similarly 
to `from_residual` is simply wrapping the output in the correct variant of 
our type. Lastly we have the `branch` method, maps values of our type to the
[`ControlFlow`][control-flow] enum to decide whether to continue execution or
short circuit.

And that is already it. Like so many things in Rust, what seems like magic 
at first is not actually all that complicated once you look into how it 
actually works. I should note that there are additional nuances to 
implementing this trait, which are outlined in the documentation. 
Specifically, it is recommended to use a newtype as the residual to prevent 
accidental cross-conversion between different kinds of residuals. And of 
course this being an unstable API, it is still subject to change.

[qmark]: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#a-shortcut-for-propagating-errors-the--operator
[result]: https://doc.rust-lang.org/std/result/enum.Result.html
[option]: https://doc.rust-lang.org/std/option/enum.Option.html
[try]: https://doc.rust-lang.org/std/ops/trait.Try.html
[from-residual]: https://doc.rust-lang.org/std/ops/trait.FromResidual.html
[control-flow]: https://doc.rust-lang.org/std/ops/enum.ControlFlow.html
