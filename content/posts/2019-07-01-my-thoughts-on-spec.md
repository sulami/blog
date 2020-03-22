---
title: My Thoughts on spec
---

In the beginning of this year I started a new job, and I am now fortunate enough
to be writing [Clojure][clj] full-time. I believe that Clojure is a very well
crafted language and enables developers like few others, but I have also some
grievances to report. I want to prefix this by saying that I love Clojure
despite its faults, and this is more of a constructive criticism piece than
anything else.

[clj]: https://clojure.org

## What Is spec?

What I want to discuss today is [`clojure.spec`][spec], the gradual typing solution
shipped with Clojure since 1.9.[^alpha] In case you are not familiar with spec,
here is a quick run-down of how it works:

[^alpha]: I am of course aware that spec is officially in alpha, and that there
    is a second alpha version which might address some of my points here. But as
    I have not tried alpha 2 yet, and a lot of people are using alpha 1, we will
    be mostly looking at alpha 1 in this post. Still, feel free to contact me
    and tell me how alpha 2 will improve things.

Clojure is a dynamically typed language, meaning no (or very few) type
annotations in the source code, unlike say Haskell or most curl-brace languages.
While this removes a lot of visual clutter, and the language is designed in such
a way that most functions can operate on most types for maximum flexibility,
this also means that sometimes things break at run time due to type errors. To
make dealing with types easier, spec allows you to place type definitions in
strategic places, say at entry- and exit points of an app or a module to ensure
that your idea of structure and type of your data still lines up with reality.

[spec]: https://clojure.org/guides/spec

The benefit of this approach over static typing is that you can encapsulate
parts of your software with type safety without having to deal with the
encumbrance in the internals. You define a contract to the outside, but stay
flexible in your implementation.

In practice it looks like this:

```clojure
(ns spec-demo
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))
            
(s/def ::name string?)
(s/def ::age pos-int?)

(s/def ::person
  (s/keys :req-un [::name
                   ::age]))
                
(s/valid? ::person
          {:name "John"
           :age 35})
;; => true

(s/explain ::person
           {:name 35})
;; => 35 - failed: string? in: [name] at: [:name] spec: :spec-demo/name
;;    {:name 35} - failed: (contains? % :age) spec: :spec-demo/person

(gen/generate
  (s/gen ::person))
;; => {:name "3o311eSXA4Dm9cLkENIt3J5Gb", :age 3318}
```

This snippet demonstrates creating a spec `person` which has sub-specs `name`
and `age`, validates some data against the spec, explains why some data does not
validate, and also generates some random data compliant with the spec. The last
bit is incredibly useful for testing, as you can imagine. There are some more
advanced features than what I have shown here, like specs for in- and output
types of functions, but this is the gist of it.

## So What Is Wrong with spec?

### specs as Macros

spec is implemented using macros, which are compile-time expansions, think
template meta-programming in C++. This brings some limitations, which are
especially noticeable in Clojure where, like in most other Lisps, code is
treated as data and many parts of your software are introspectable at run time.
specs are, once defined, not easy to extract data from.

Contrast this with [schema][schema], a Clojure library which does essentially
the same thing, and got started before spec. schema "schemas" are just hash
maps, which are easily constructed and introspected at run time. schema has
other problems, but the community seems to generally flock to spec as the
officially blessed version.

[schema]: https://github.com/plumatic/schema

The situation I have now repeatedly seen is one where I have a spec defining a
specific representation of an entity, where for example one spec extends another
one. A practical example here is when augmenting an entity with additional data,
so that the output representation is a modified version of the input type.

You can define a spec as a superset of another spec, and only in that direction,
using `s/or`, but at that point you are already stuck in the spec an cannot get
the list of fields easily out of your specs. It would be nice to be able to
define one spec in terms of flat data, which would also be available at run
time. This would be incredibly helpful for use with something like
`select-keys`.

There are ways of working around this, for example by using `eval`, or wrapping
your spec macros in another layer of macros, but both of these are more hacks
than solutions. [spec alpha 2][alpha2] promises to solve this particular problem
by separating symbolic specs from spec objects, the former being just flat data
which can also be generated programmatically.

[alpha2]: https://github.com/clojure/spec-alpha2/wiki/Differences-from-spec.alpha

If spec alpha 2 works out the way I hope, this will be solved soon, and the
documentation looks promising so far.

### Namespacing

spec is very intent on you using namespaces for your keywords, which in general
is a good idea. Many developers, including myself, have been using the namespace
of a key to give context about its use, for example `:person/name` is a
different spec from `:company/name`. The problem with this is the overloading of
namespaced keywords, which are an existing Clojure feature. These namespaces
`person` and `company` do not actually exist, and I do not want to clutter all
my keys with a long prefix like `:myapp.entities.person/name` to make it match a
real namespace.

Now namespaced keywords can have any namespace, and the namespace does not
actually have to exist for everything related to keywords to work well, except
when it does. If you want to use a spec from another namespace, but `alias` that
namespace to shorten it, you need to `require` that namespace, for which it
needs to exist. As a workaround to this I have created "fake" namespaces in the
past using a helper.

This actually leads me to another problem, which is the question of where to
place specs in the first place. spec comes with a global, centralised registry
for specs, which in alpha 1 you cannot opt out of. In theory this allows you to
define/register your spec once in any place you like, and then access it from
anywhere without having to know where it comes from or requiring it. This, while
having the potential for being too opaque and magical, is actually a very good
feature in my opinion. It is trivial to override specs when reloading them, and
I have not experienced any issues with evaluation order yet. Due to specs
referring to other specs by their name, you can define dependencies of a spec
after the fact, and the system will pick them up accordingly.

My current solution for this is having a file & namespace for every entity which
can be required and aliased normally, the only problem with this being
relationships between specs. As soon as one spec needs to include another spec,
dependencies get muddled, so I have experimented with having a special namespace
for specs which are shared across many other specs, but this is far from ideal.
I wish there was a cleaner way to do this, especially leveraging the global
registry.

### Function spec Definitions

I mentioned above that spec also allows instrumenting functions, but the
semantics for this are a bit wonky in my opinion. See for yourself:

```clojure
(defn double [x]
  (* 2 x))
  
(s/fdef double
  :args (s/cat :x int?)
  :ret int?
  :fn #(= (:ret %)
          (-> % :args :x (* 2))))
```

This naive spec restricts in- and output types to integers, which is okay in
this case. The `:fn` key describes the relationship between in- and output, and
is in this case actually absurdly strict, but this is just an example. There are
two issues I have with this:

First the `:fn` definition tends to be very elaborate and hard to understand at
a glance. Even in this simple case, there is a lot going on in there, and the
use of anonymous functions does not help it. In practice, this key is optional
and I omit it almost always, because I cannot think of any formal assertions I
want to make about the output which are also reasonably simple to encode. And if
you want to make several separate assertions about the output, you almost cannot
avoid breaking up the spec into pieces, at which point you have predicates which
exist purely for spec.

The other issue I have is that this is decoupled from the actual function
definition. In theory you can place the spec in a different namespace and refer
to the function using its fully qualified name, and this is tempting, especially
when looking at my previous point about these specs having the potential to be
far more longer than the actual function definitions. But then your function
exists conceptually in two places, and these two places have to be kept in sync.
If you move, rename, or modify the function in almost any way, you have to
modify the spec, too, but first you have to find the spec.

The problem I can see with this is that `:fn` can be used to make assertions
about the functions, which can in almost all cases be made in unit tests as
well. In fact, unit tests are meant for exactly this, asserting single
assumptions about units at a time. The condition above could just as well be a
test called `the result equals twice the input value`. Functions are usually
only instrumented locally and/or during testing, as they incur non-negligible
overhead, and I would argue that they do not provide significant assurances over
unit tests.

I would much rather drop the `:fn` key, and include type declarations in the
actual function definition, which is incidentally how schema works:

```clojure
(s/defn double :- s/Int
  [x :- s/Int]
  (* 2 x))
```

In the end, I am looking forward to the next iteration of spec, and hope it
addresses as many issues as possible, and I am already blown away by how much
better it is than alternatives available in other languages.
