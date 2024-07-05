title = "Synchronous Core, Asynchronous Shell"
slug = "sync-core-async-shell"
timestamp = "2024-07-05"
tags = ["rust", "software-design"]
---

A bit over a decade ago, Gary Bernhardt published [Functional Core, 
Imperative Shell][fcis]. His proposed software architecture uses functional 
programming style, especially immutable data, for the bulk of the logic, and 
an outer shell that uses imperative programming style for side effects such 
as I/O. This makes it easier to understand, test, and change the logic 
without imposing the difficulties of entirely functional programs. Gary also 
presented an expansion of this idea in his talk [Boundaries][boundaries] 
later that same year. I personally like and use this pattern, and think it 
is much more universally useful than for example pure functional or 
object-oriented styles.

More recently there has been a lot of discussion about the ergonomics of 
Rust's `async` functionality, with many participants expression frustration 
about the boundary between synchronous and asynchronous parts of the 
language. A common complaint references [function colors][colours], the idea 
that asynchronous parts don't compose well with synchronous parts due to 
their different execution model. Specifically, asynchronous functions can 
call synchronous ones, but not the other way around, at least not without 
additional ceremony.

Looking at most real world Rust services[^1] I work on these days, a lot of 
them perform asynchronous I/O using [Tokio][tokio], especially interactions 
over the network such as data stores and other services. It struck me that I 
tend to follow a _synchronous core, asynchronous shell_ pattern, which uses 
the same ideas as Gary laid out earlier, but additionally directly maps 
synchronous and asynchronous code into the core and shell, respectively. 
This comes with all the same advantages of the original, and avoids a 
situation where synchronous code calls asynchronous code, as the core does 
not call the shell.

Furthermore, I would argue that function colouring is actually a positive 
aspect, as it provides a guardrail against carelessly introduced side 
effects in the core. A better way to trigger side effects from the core, 
should you really have to, is to use the [command pattern][command] to 
return a description of a side effect as data instead of calling directly 
into asynchronous code.

[^1]: I also work on a decent amount of embedded and desktop applications, 
which are often entirely synchronous, though with [Embassy][embassy] even 
embedded software is getting an asynchronous shell.

[fcis]: https://www.destroyallsoftware.com/screencasts/catalog/functional-core-imperative-shell
[boundaries]: https://www.youtube.com/watch?v=yTkzNHF6rMs
[colours]: https://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/
[tokio]: https://tokio.rs/
[command]: https://en.wikipedia.org/wiki/Command_pattern
[embassy]: https://embassy.dev/
