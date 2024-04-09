title = "Programming Language Scalability"
timestamp = "2024-04-09"
tags = ["staff"]
---

I have been thinking about programming language scalability. I am not talking
about performance here, but organizational scalability. Let us start with a bit
of background.

One of the reasons I originally joined CircleCI was because they were using
Clojure, and I had been dabbling in Clojure at the time.[^1] I believe their
choice of language caused a self-selection effect for many of the early
engineers, probably for the better. Regardless, some time before I left the
company, we got the mandate to move everything to Go. The company was going
through an intense growth phase, and it was becoming increasingly difficult to
hire Clojure engineers. We had a reasonable amount of Go code already,[^2] so
the choice was obvious. Another factor that played into this decision was the
fact that most Clojure engineers could drop into a Go service and make some
changes relatively easily, while the reverse was not quite as true. This is not
because the Clojure engineers were better, but because it is more difficult to
be effective in an unfamiliar Clojure codebase than in a Go one.

There were also some technical growth pains we experienced on the Clojure side.
Clojure being a highly dynamic language, we had a decent number of runtime
errors, which we could only counter with additional testing and observability.
The lack of static types also made development more difficult for anyone not
directly familiar with the code, as almost everything tended to be a bag of
values in the form of a map.[^3] The macro system and a culture that emphasized
expressiveness led to a variety of different design patterns, each optimized for
their respective author's preferences, and inscrutable to everyone else. It also
did not help that Clojure has no standard approach to structure a service, so
everyone rolled their own. One could not freely jump between different Clojure
services without having to learn the different idioms.

In my current day job, we are running a Ruby on Rails monolith. The engineering
department is much smaller at around 30 people, but the monolith has been around
for about a decade. While Rails engineers are easier to hire than Clojure ones,
especially here in Japan, we are experiencing some of the same pain points as we
did at CircleCI. Ruby is also dynamically typed, and has a culture of
domain-specific languages that read almost like plain English. "Automagical"
abstractions are seen as a virtue. As a result, we have a variety of competing
patterns for almost every situation, each of which requires some learning.
Within the monolithic architecture, no boundaries are defined or enforced, and
it is difficult to reason about the impact of any given change.[^4] What
personally gets me is the meta-programming and module system which together make
it difficult to track down where methods come from, even for LSP servers.

In both of these cases, the problems maybe could have been alleviated with
better technical leadership or otherwise different practices, but I believe in
setting yourself up for success despite yourself. As organizations grow, one
cannot depend on everyone being good at their job, or even average, if
statistics are to be believed. With this I would like to talk about the
scalability of a programming language, which I will define it as:

> A programming language is more scalable if an engineer unfamiliar with a code
> base written in it produces correct code more quickly.

Scalability is often at odds with _peak effectiveness_, the maximum
effectiveness of an engineer who is intimately familiar with the codebase,
because the features driving peak effectiveness are often enabling abstractions
tailored towards the specific use case at hand, like macros and support for
domain-specific languages. These abstractions can make domain experts more
effective, but present an additional barrier to entry to everyone else. At the
extreme end of this spectrum sit code golf languages.

There are many factors that contribute to scalability. Prevalence determines how
easy it is to hire for, and to some extent also the support from the ecosystem
in the shape of libraries, tooling and available knowledge. Guardrails help
prevent mistakes, the biggest ones being memory safety and static type checking.
The latter also enables better support for editor integrations such as LSP which
enhances discoverability. Standardized tooling for the build process,
formatting, testing, and so on further reduce the potential need to learn
multiple approaches. Support for, and a culture of heavily using language
extension features on the other hand reduce scalability, as detailed in the
[Lisp Curse](http://www.winestockwebdesign.com/Essays/Lisp_Curse.html).
Languages that deviate a lot from the mainstream are on average more difficult
to learn, whether it is quirky syntax with a lot of parentheses, or a weird
execution model that involves monads.

To be clear, language scalability is not just something managers want to
optimize for in order to ensure engineers are fungible. While independent teams
are a worthy goal to aspire to, in reality cross-team dependencies are common,
and being able to submit a change to another team's code can reduce the overhead
incurred. And of course some code goes untouched for many months or years, to
the point where even the original authors cannot claim familiarity anymore.

As an example, I have collected some subjective thoughts on the scalability of
programming languages I am familiar with, ordered roughly from most scalable to
least scalable, in the context of web service backend systems. In other contexts
the list would of course look different.[^5]

<details>
<summary>
A non-exhaustive list of programming languages
</summary>

- Rust
    - Prevalent enough and backed by several corporate sponsors to be considered mainstream
    - Fantastic support through a strong type system and LSP integration
    - Great build tooling through cargo
    - Concise but not obtuse
    - Steepest learning curve among mainstream languages
- Go
    - Ubiquitous, backed especially by Google
    - Very easy to pick up
    - Statically typed and emphasis on explicit code
    - Pushes a lot of complexity onto the programmer under the guise of simplicity, in my opinion overdoing it
    - Some questionable design decisions, like `nil` and error handling ergonomics, but also the module system and [lots of small things](https://fasterthanli.me/articles/i-want-off-mr-golangs-wild-ride)
- Java/Kotlin
    - Ubiquitous with many corporate sponsors
    - One of the best library ecosystems
    - Great tooling available, especially IDEs and profilers
    - Prone to overly complex design patterns
    - Build tooling is aged
- Python
    - Ubiquitous
    - Very easy to pick up
    - Despite officially being multi-paradigm generally quite standardised
    - Great library ecosystem
    - Dynamically typed,[^6] and no boundary enforcement; anything can be monkey-patched from anywhere
- Ruby
    - Prevalent enough to be considered mainstream (though waning), backed by several corporate sponsors, such as Stripe and Shopify
    - Dynamically typed, and even worse boundary enforcement than Python
    - Culture of meta-programming and domain-specific languages
- JavaScript/TypeScript
    - Ubiquitous
    - TypeScript has one of the better type systems
    - Quite well optimised, with different runtime options available
    - Designed as a scripting language for web browsers, with shortcomings arising from that
    - Many layers of abstractions built up over time, especially conventions
    - Prone to paradigm shifts every few years
- Erlang/Elixir
    - Quite niche for the general purpose, although used in some selected contexts
    - Dynamically typed
    - Ecosystem somewhat limited for general purpose use
    - Actor-model and servers take some learning
- Clojure
    - Available on the JVM, CLR, or JS runtimes, and has decent interoperability to leverage those ecosystems
    - Very niche
    - Dynamically typed[^7] with `nil` propagation issues
    - Lisp-like culture of macros and domain-specific languages
- Haskell
    - Statically typed
    - Very niche
    - Hard to learn, even harder to master
    - Complexity scales badly with program size
    - Almost requires domain-specific languages with custom operators
</details>

If I wanted to select viable candidates from that list, I would probably pick
between Rust, Go, or Java/Kotlin. C# would probably also work, but I have not
written any in close to a decade. I arrive there with a simple heuristic, as all
of these languages are statically typed and considered mainstream, almost
boring. I am excluding C and C++ for the lack of guardrails, and Python and Ruby
for the lack of static typing. Typescript as a language might be fine, but I
personally have doubts about the stability of the tooling, which might be
unfounded. It is certainly popular, even on the backend.

Programming language scalability is only interesting for organizations, and only
beyond a certain size. If your engineering team can fit in a single room for the
next five years, it probably does not matter. Conventional wisdom is that the
best technology to start a new project is the one you know well. And sometimes
there will be technical requirements that trump organizational ones. In any
case, you should take care to spend your [innovation
tokens](https://mcfunley.com/choose-boring-technology) wisely.

[^1]: Though I've never met him, I've been told Clojure was chosen because Paul
    Biggar liked it at the time. He's now making
    [Darklang](https://darklang.com/), which looks interesting as well.
[^2]: This also predates my tenure, but when the CircleCI CLI was written,
    Clojure's significant application startup time disqualified it for CLI
    tools. In plain Clojure, depending on your dependencies, you might wait 10
    seconds or more while the runtime is loading before your program even starts
    doing anything. Not a big problem for server-side processes, in fact the
    original CircleCI monolith took several minutes to load. Nowadays one could
    use [GraalVM](https://www.graalvm.org/) to get around this. From what I've
    been told, Go and Rust were in the running, and at the time Go made it
    easier to produce a statically linked binary, so it won. By the time the
    switch was mandated, I reckon almost half of all code running was Go.
[^3]: In my area in particular there were probably a dozen or more different
    shapes of maps one would call "a build," all of which had different sets of
    keys. One would then go and inspect callers to figure out which one was
    actually being passed. In some cases it wouldn't even be a single one.
[^4]: I'm aware of projects like
    [Packwerk](https://github.com/Shopify/packwerk), but besides the
    non-negligible maintenance overhead, it only solves parts of the problem.
[^5]: If you have platform requirements, the list might actually be very short.
[^6]: Technically there are type annotations and third-party type checkers are
    available, but as far as I know they are not widely used, and by far not as
    useful as is the case for statically typed languages.
[^7]: I'm aware of spec/schema, but at least at CircleCI we did not consistently
    use these. They also don't tie well into tooling at all for the purpose of
    static analysis, and mostly just cause exceptions to actually be thrown at
    the source of the problem instead of somewhere else random.