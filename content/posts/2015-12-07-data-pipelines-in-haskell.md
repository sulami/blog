---
title: Data-Pipelines in Haskell
---

Today I was writing some code in Golang and thought about why Haskell feels so
much more powerful and expressive. Then I had a revelation. Haskell follows the
UNIX philosophy, focussing on small functions that do exactly one thing (at
least good Haskell code does so). It also makes these functions easily
combinable without leading to syntastic noise like parenthesis-mountains. So I
thought I might just write a post highlighting that, which is what you are
currently reading.

We are going to start at the very basics, assuming you have little to no prior
knowledge about Haskell. Haskell uses the [lambda calculus][0] and just
whitespace to declare function arguments. So a function call looks like this:

```haskell
add 2 3 -- 2 + 3 = 5
```

If one of your arguments is actually the return value of another function call
with arguments, you will need to add parentheses to tell your compiler which
arguments belong to which function call, like this:

```haskell
add 5 (add 1 3) -- 5 + (1 + 3) = 9
```

Haskell functions often take arguments in an order so that the last argument is
often the "main" one that is being used, which plays nicely with the `$`
operator that is part of Prelude, the part of the Haskell standard library that
gets imported automatically. `$` has the type ` (a -> b) -> a -> b` which looks
sort of pointless, but in practise you can use it to build function pipelines
without having to use parentheses.

```haskell
add 5 $ add 1 3 -- 5 + (1 + 3) = 9
```

Note that in this code, `add 5` is a function that takes one argument, because
one is already supplied. Its type is `Int -> Int` compared to the original `Int
-> Int -> Int`. And because Haskell has easy function composition using the `.`
operator (`(b -> c) -> (a -> b) -> a -> c`), we can chain several functions in
front of a `$`, building a reverse data pipeline like this:

```haskell
add 5 . subtract 3 . times 2 $ add 1 3 -- (((1 + 3) * 2) - 3) + 5 = 10
```

As you can see, the data flows from the right side to the left. But coming back
to the UNIX philosophy, there are also data pipelines, pipes. But in
shell-pipes, data flows from left to right, which might be more intuitive. Note
that in the last explanation I had to reverse the order because of the
subtraction. We can fix this quite easily by defining a new operator, the
"reverse `$`":

```haskell
(&) :: a -> (a -> b) -> b
x & f = f x
```

Note that the type annotation is the same as the one of `$` with just the
arguments reversed. This specific operator is already defined in many
community-made packages. It allows us to mimic the shell-pipe very closely:

```haskell
add 1 3 & times 2 & subtract 3 & add 5 -- (((1 + 3) * 2) - 3) + 5 = 10
```

Now the explanation looks just like the code, and data flows from left to
right, just like in the shell. These data pipelines make it easy to let small
functions like `words`, `lines` or `map` work on data and pass the results to
the next function, without having to worry about memory management, buffer
overflows or even types, which makes this more robust than a shell-oneliner
while being just as expressive and simple. Haskell's laziness even allows for
simple parallelism like the shell does when piping data, where
functions/programs start working as soon as the first data reaches them.

This concept is so great, there is even a huge library to do this while doing
I/O like when accessing files or communicating over a network, [Conduit][1].

  [0]: https://en.wikipedia.org/wiki/Lambda_calculus#Lambda_calculus_and_programming_languages
  [1]: https://www.fpcomplete.com/user/snoyberg/library-documentation/conduit-overview

