title = "What is Type-Level Programming?"
slug = "type-level-programming"
timestamp = "2023-05-02"
tags = ["rust", "software-design"]
---
In programming languages with an emphasis strong type systems, such as Rust or Haskell, there exists a technique called type-level programming. I think TLP is an interesting solution for some class of problems, so I am sharing a very brief introduction to the concepts involved.

If one hangs out in the right circles, you might have heard of dependent types, such as in [Idris](https://www.idris-lang.org/). The Wikipedia definition of dependent types is "a type whose definition depends on a value." This can be useful, if we want to align our types with real-world constraints.

While I have written my fair share of Haskell, I first explicitly came across TLP in embedded Rust, specifically in the [embedded-hal](https://docs.rs/embedded-hal/latest/embedded_hal) crate, which uses types to distinguish hardware pin modes. I am going to adapt this example to showcase the benefits.


# Shifting Left

But first, a bit of theory. In regular programming, a type is a set of values, so a `u8` is integers between zero and 255. With static typing, we can know the type of a value at any time without having to run the program.[^1] This is considered useful because it prevents a class of errors from happening at program run time, where they could cause harm, the idea being that you would rather have the compiler complain than the program crash and kill people.

Now sometimes it might be useful to know not just the type, i.e. the set of possible values, but also the specific value of a variable prior to running the program, also to prevent bugs. Languages such as Rust and Haskell not only have types as sets of values, but also sets of types, which are called traits and type classes respectively.[^2] I will use the term traits here for brevity. Traits can also be determined prior to running a program, and can act in the same way as types to say "this is an element of this set," but the set is now a set of types, not a set of values.

Because types are also known at the same time, by "shifting left" our values to be types, and our types to be traits, we have gained complete knowledge of our program without running it. If we know the "value" of a variable prior to running the program, we have actually computed the variable ahead of time.[^3]


# Embedded Systems

Coming back to embedded-hal, I mentioned pin modes. One of the important factors in embedded development is that one has limited access to the environment a program runs in, which makes debugging more cumbersome, and errors can often be irrecoverable.[^4] This explains why some additional steps are taken to improve program correctness, even at the cost of ergonomics in some cases.

In the case of many micro-controllers, such as any of the popular Arduino family, one has to declare [the mode](https://docs.arduino.cc/learn/microcontrollers/digital-pins) of a GPIO pin, usually one of several different flavours of in- or output. This changes the behaviour of the pin on an electrical level, and is important to get right to get the right behaviour, and avoid releasing the magic smoke.[^5]

The [standard code](https://reference.arduino.cc/reference/en/language/functions/digital-io/pinmode/) for this in the Arduino variant of C++ is:

```c++
void setup() {
  pinMode(13, OUTPUT);    // sets the digital pin 13 as output
}

void loop() {
  digitalWrite(13, HIGH); // sets the digital pin 13 on
  delay(1000);            // waits for a second
  digitalWrite(13, LOW);  // sets the digital pin 13 off
  delay(1000);            // waits for a second
}
```

This looks fine, but what happens if you do not get the pin mode right, for any of many possible reasons? `digitalWrite` still does [something](https://www.arduino.cc/reference/en/language/functions/digital-io/digitalwrite/), but not what you want it to do. This could be a difficult to track down bug.

[In Rust](https://github.com/Rahix/avr-hal/blob/main/examples/arduino-uno/src/bin/uno-blink.rs) on the other hand&#x2026;

```rust
let mut led = pins.d13.into_input();

loop {
    led.set_high();
    arduino_hal::delay_ms(1_000);
    led.set_low();
    arduino_hal::delay_ms(1_000);
}
```

&#x2026; this bug becomes a compile-time error:

```rust-compilation
LL  |     led.set_high();
    |         ^^^^^^^^ method cannot be called on `Pin<Input<Floating>, PB5>` due to unsatisfied trait bounds
[...]
LL  | pub struct Pin<M, I>
    | --------------------
    | |
    | doesn't satisfy `_: _embedded_hal_digital_OutputPin`
```

The key here is the first line, specifically the `.into_output()` call. This method consumes the pin, which has a type of `Pin<Input<Floating>, PB5>`,[^6] and returns a new pin of a different type `Pin<Output, PB5>`. Only this type implements the `OutputPin` trait, and thus actually has the `.set_high()` and `set_low()` methods defined for it. This allows one to write a function that takes for example any pin in the floating input mode, or just a specific pin in a specific mode.

An added benefit here is that no run time checking is required, which saves valuable CPU cycles, as well as no error handling or recovery code, which would be more work and hold the potential for more bugs.


[^1]: I'd say "at compile time," but one can totally have static types without a compilation step. `cargo check` is kind of that, it will type check, but not compile. LSP will also tell you the type of a variable for many languages, such as Typescript.

[^2]: Typescript's union types are actually an example as well.

[^3]: The same would be true if we hard-coded a constant. We know the variable has a known value, so we can avoid any bugs related to it having the wrong value.

[^4]: Not to mention fatal, in the case of many micro-controllers in charge of hardware that can potentially kill.

[^5]: The one you see if you fry one of your components.

[^6]: There is a lot going on here, this pin actually has a type that contains not only the mode of the pin, but also the specific pin number. This means pin identity is fixed on a type level, and such at compile time.
