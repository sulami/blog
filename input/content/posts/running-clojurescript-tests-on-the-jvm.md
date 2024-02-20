title = "Running Clojurescript Tests on the JVM"
timestamp = "2018-10-01"
tags = ["clojure"]
---
Recently I have been writing a lot of Clojure and Clojurescript for my side projects. This is my first post on the language & ecosystem, so I what follows below might be very wrong in places. Nontheless I want to share something I worked out myself and might be useful to someone else. The topic we're going to be looking at is running (unit) tests for a [re-frame](https://github.com/Day8/re-frame)-based Clojurescript application, though it actually applies to almost any CLJS application.

Last week I was finally adding tests to one of my side projects[^1] and I was wondering how to run them easily. Clojure comes with a great bulid tool, [leiningen](https://leiningen.org), which allows you to run tests quite easily, at least for Clojure code.


# Ways to Run Tests

Now the way I run my code is by compiling my code to Javascript, launching a browser session through Leiningen, and running my code in the browser's runtime. This works very well for actually running the code using [figwheel](https://github.com/bhauman/lein-figwheel), but less well for running tests.

The first reason this is a bit impractical is because it requires a lot of setup to actually make it work, you need to update your `project.clj` to have a testing build, which needs proper cleaning up as well, and then somehow manage the browser runtime as well. Depending on how you do this, this also incurs heavy startup time penalties.[^2]

To avoid the browser, which is just unnecessarily heavyweight if all you need is just a JS runtime, the go-to solution used to be [PhantomJS](http://phantomjs.org), a headless browser that can be used for various tasks that do not require an actual human seeing rendered output. Sadly, PhantomJS suspended development earlier this year due to lack of active contributors, so I am not very confident in building on top of it, especially considering the speed at which the web and standards are currently changing.

While PhantomJS comes with a whole DOM emulation, what we are actually looking for is only a JS runtime, for reasons that will become apparent later. This leads us to [Node.js](https://nodejs.org/en/), which is exactly that, a stand-alone JS runtime. Running tests inside Node is quite a viable option, as it does not impose any meaningful startup time penalty by itself, meaning we can just start it up fresh for every test run and not [have to deal with persistence and state](https://twitter.com/garybernhardt/status/1007699556832817152).

We could stop there, write a custom test build target and a custom test command that just runs that file for Leiningen and be done with it. But we still have the compilation time to JS which is just too long if you want to have a very tight TDD loop. Then I realised that we do not actually need to compile to JS at all, in the end we just want to test Clojure code, which can run on a variety of different platforms.

One of the fastest platforms available to us is the original platform Clojure targeted, the JVM. Because I am using Emacs with [CIDER](https://github.com/clojure-emacs/cider), I already have a long-running JVM[^3], I do not even have to consider the ~1s startup time the JVM incurs. This setup allows us to run tests in well under one second, but leads me to the next section.


# The Theory: Decoupling Logic from Presentation

If we want to run our Clojure code, which does not specify a target platform, on the JVM, we cannot depend on any platform-specific features. This means, our Clojurescript application needs to be split into platform-independent Clojure code and Clojurescript glue code. Incidentally, this is also the structure TDD favours and I think that leads to the code easiest to reason about.

re-frame does come with an [example of how to expose event handlers for tests](https://github.com/Day8/re-frame/blob/master/docs/Testing.md#exposing-event-handlers-for-test), which are the part of our application that should contain the bulk of the application logic. The basic gist is this: any kind of state should be mutated by passing it into a pure function, which just returns the new, modified state. Every variable is state.[^4] Then use the most basic glue code you can come up with to glue this together.[^5]

This allows us to then test the application logic in isolation, by just passing a piece of state into a function and checking properties on the returned value. If it makes sense, we can also use properties to randomly generate a huge amount inputs according to some rules and make sure that we do not forget any edge cases we did not think of writing tests for. Because none of this code actually depends on anything related to the presentation, it does not matter whether it runs in a browser or on the JVM.


# The Practice: The Implementation of re-frame Tests

In order to be able to run Clojure code on the JVM, it cannot be Clojurescript code, meaning it cannot reside in `.cljs` files and cannot use CLJS-specific libraries, but some of your code will invariably be specific to Clojurescript, like for example

```clojure
(ns foo.bar
  (:require [cljs.spec.alpha :as s]))
```

needs to be

```clojure
(ns foo.bar
  (:require [clojure.spec.alpha :as s]))
```

to work on the JVM. This can be solved quite easily by using [reader conditionals](https://clojure.org/guides/reader_conditionals). The basic idea is to save your application logic in Common Clojure (`.cljc`) files, write the wrapper around your application in Clojurescript (`.cljs`) and your test suite in Clojure (`.clj`). Within Common Clojure you have access to reader conditionals which are a feature in Clojure >= 1.7 which allow modifying code at "compile-time"[^6] depending on the compilation target. This is what it looks like in action:

```clojure
(ns foo.bar
  (:require #?(:clj  [clojure.spec.alpha :as s]
               :cljs [cljs.spec.alpha :as s])))
```

Once we are at this point, writing the actual tests is quite easy. I am going to borrow an example from the [re-frame docs](https://github.com/Day8/re-frame/blob/master/docs/Testing.md) here:

```clojure
(let [
      ;; setup - cummulatively build up db
      db (-> {}    ;; empty db
             (initialise-db [:initialise-db])   ;; each event handler expects db and event
             (clear-panel   [:clear-panel])
             (draw-triangle [:draw-triangle 1 2 3]))

      event  [:select-triange :other :stuff]

      ;; now execute the event handler under test
      db'    (select-triange db event)]

      ;; validate that db' is correct
      (is ...)
```

In this example, `db` is the global state map, which gets passed into a chain of pure event handler functions, after which the returned new state can be validated. All we need to do to run these is set a `:test-paths` setting in `project.clj` and `lein test` will pick up the tests.


[^1]: Way too late as well, I should have [obeyed the testing goat](https://www.obeythetestinggoat.com). I had quite some trouble untangling my wild-west design to be able to even write proper unit tests.

[^2]: Full test compiles from CLJS to JS for a small test suite (~20 test cases) already takes about 5-10 seconds.

[^3]: I know we are back to long-running processes, which is a bit unfair, but CIDER allows me to basically instantaneously reload the whole project, which so far worked without any hiccups.

[^4]: Ideally abandon the idea of a variable as well, default to immutable values everywhere.

[^5]: This is really just functional programming, which has been around for over half a century.

[^6]: I do not actually know in depth how this works behind the scenes yet, but I believe this is a fair approximation even if potentially inaccurate.
