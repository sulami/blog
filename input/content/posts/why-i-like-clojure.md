title = "Why I like Clojure"
timestamp = "2019-08-30"
tags = ["clojure"]
---
This is somewhat of a response to Uncle Bob's [post of similar nature](http://blog.cleancoder.com/uncle-bob/2019/08/22/WhyClojure.html), which I would say has gotten a mixed to positive reception. I had planned a similar post a week or two before the release of his, but archived the idea upon reading his post. But after having read it over a couple of times I have now decided that I still have something meaningful to write. What follows are the purely subjective reasons for which *I* enjoy using Clojure. Some have criticised Bob for being very absolute and not giving up any screen estate for more nuanced viewpoints, something I will try to avoid.


# Lisp

There is not much to say about this that has not already been said. The homoiconicity, meaning code can be represented as a data structure inside the same language, extensibility through macros which can modify both the evaluation order as well as the very syntax to the point where [it looks more like LaTeX than Lisp](https://docs.racket-lang.org/scribble/getting-started.html).[^1]

This also means that you are not stuck with a paradigm. While OO seems to be out, and FP the new hotness, Lisp can do them all, and historically often did before anyone else. Bob mentions dynamic typing in his signature retorts to (I am guessing) fictional counter-arguments, and he is right to mention [`clojure.spec`](https://clojure.org/guides/spec), a library for gradual typing (omitting [`schema`](https://github.com/plumatic/schema), an alternative). Racket has a [fully typed variant](https://docs.racket-lang.org/ts-guide/quick.html), there is something that is basically [Haskell in a Lisp-bun](https://shen-language.github.io), and let us not forget that there is actually [Typed Clojure](https://github.com/clojure/core.typed), with static type checking and all.[^2]

Being able to generate code without being stuck on a "dumb" level by generating strings and passing them into `eval` like for example in Python allows for sane hyper-dynamic programming, where the program adapts itself to the conditions. Being able to read and write code in a safe manner enables extremely powerful tooling in the Lisp world. Linters are very smart, because [reading code into a data structure is trivial](https://github.com/xsc/rewrite-clj), usually a one-liner, and there are many more tools to automatically rewrite and refactor code than for other languages.

Now I do not want to discount the [Lisp Curse](http://winestockwebdesign.com/Essays/Lisp_Curse.html), it is a real thing, and one of the reasons that while Lisp has stuck around for over half a century, it has not made it into the mainstream. The other main factor probably being the performance problems due to the gap between software and hardware architecture.[^3] But with the advent of the internet, ecosystems like GitHub, and hardware that is fast enough that we consider running basically full web browsers for half of our applications[^4], I think that these issues have become surmountable.

I do not think I need to mention how incredibly useful the REPL and hot-loading code into a running system are?


# Hosted

Clojure is explicitly designed as a hosted language, which I think was a very good move. If you are writing a new language today, it might be better than the established ones, but the cost of leaving an existing ecosystem of libraries and Stack Overflow answers just because the new language is 5% nicer is not a trade off many people will want to make. Clojure being hosted and having excellent interoperability with its host platform means it can benefit from existing ecosystem, let alone platform implementations.[^5]

While the primary platform is the JVM, superbly uncool but stable and relatively performant, there is a CLR (.NET) version which is "almost even on features" thanks to Davit Miller, as well as a very mature JavaScript version in the shape of ClojureScript. The JVM (and to some extent the CLR) have excellent support by big software vendors, if you buy some kind of software with an API, chances are there is a Java SDK which you can use easily from your Clojure code. The JavaScript ecosystem is the largest in numbers[^6], and includes Electron and React-Native, both of which can be used with some, but not unreasonable, effort from ClojureScript code. One of the newest additions has been GraalVM, which while not 100% feature-complete yet, already allows compilation to native static binaries of many Clojure programs[^7], running without the JVM at all, and doing away with the dreaded multi-second startup time.[^8]

The platform split could have been one of the big, curse-like problems for Clojure, but there is Clojure Common, used by many popular libraries, which allows you to write platform-independent code by using conditional branching for all platform-specific code.


# Community

Despite all the positive points I mentioned, Clojure is still a niche language, and in some way that is good as well. Sure, finding jobs is harder[^9], but not impossible. Clojure developers, like for example Haskell or Rust ones, tend to be more experienced, as it is not a typical first language, and requires a certain interest in the craft. Many Clojure developers have written widely used tools and libraries, not just in Clojure, but also for example for Emacs, which is understandably quite popular with Clojurists.

Rich Hickey himself, the BDFL of Clojure, is someone with decades of industry experience and a desire to get it right. I think he is doing a pretty good job. While there are some small inconsistencies in places, the bulk of the language, and all the important parts are very well thought out.<sup><a id="fnr.10" class="footref" href="#fn.10" role="doc-backlink">10</a></sup> Clojure is a very stable language, which means that smaller problems will stick around for a while, but also means you can trust that your code will not break every time you update your dependencies.

In the end, it comes down to enjoyment. I enjoy working with Clojure. I feel like there is a lot to learn, and the language is inviting me to explore beyond the current possibilities of software development. I feel good about the elegant and concise solutions<sup><a id="fnr.11" class="footref" href="#fn.11" role="doc-backlink">11</a></sup> I can come up with. It has changed the way I think, in a good way.


[^1]: Racket is actually a great example of the flexibility of Lisp. A language designed to build other languages in it, call it a meta-language. Even package import can be wrapped up in a "language" trivially, meaning that you can essentially write a tiny DSL for every project. Not saying that is necessarily a good idea, but you can.

[^2]: There are some issues with it, namely that the coverage is not all that great, but it exists and works, meaning if you really *need* static types, you can get them.

[^3]: There were Lisp machines, which had hardware tailored towards running Lisp, but they never took off either.

[^4]: Looking at you, Slack.

[^5]: Do you really want to implement your runtime for FreeBSD on a smart toaster oven? Raspberry Pis are non x86, BSD is not Linux, and who knows what is up with Windows. This matrix is growing quickly.

[^6]: In part due to left-pad-like five-line-packages, but still.

[^7]: [Zprint](https://github.com/kkinnear/zprint) is one of those CLI tools that takes advantage of the reduced startup time.

[^8]: I am planning to write a piece about GraalVM some time later this year.

[^9]: Large companies like Walmart and CircleCI (my employer) are Clojure shops, so it is far less obscure than one might think.

<sup><a id="fn.10" class="footnum" href="#fnr.10">10</a></sup> We can also see right now how `clojure.spec` is being adapted after community feedback to the first alpha version, which has been available for about 1½ years.

<sup><a id="fn.11" class="footnum" href="#fnr.11">11</a></sup> concise ≠ obtuse
