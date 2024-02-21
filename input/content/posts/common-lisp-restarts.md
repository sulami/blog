title = "Restarts in Common Lisp"
slug = "common-lisp-restarts"
timestamp = "2020-04-01"
tags = ["lisp"]
---
***Errata:** An earlier version of this post was misrepresenting conditions as exceptions, which has been addressed.*

---

I have been reading *[Practical Common Lisp](http://www.gigamonkeys.com/book/)* by Peter Seibel over the weekend, which is an excellent introduction to Common Lisp, showcasing its power by writing real programs. If you are interested in Lisp or programming languages at all, I recommend at least skimming it, it is free to read online.

Writing a Lisp-descended language professionally, and also living inside Emacs, I had dabbled in Common Lisp before, but I still found something I was not aware of, restarts. I do not think that this is a particularly well known feature outside the Lisp world, so I would like to spread awareness, as I think it is a particularly interesting take on error handling.

The book explains restarts using a mocked parser, which I will slightly modify for my example. Imagine you are writing an interpreter/compiler for a language. On the lowest level you are parsing lines to some internal representation:

```lisp
(define-condition invalid-line-error (error)
  ((line :initarg :line :reader line)))

(defun parse-line (line)
  (if (valid-line-p line)
      (to-ir line)
    (error 'invalid-line-error :line line)))
```

We define a condition, which is similar to an exception object with metadata in other languages[^1], and a function which attempts to parse a single line.[^2] If it turns out that the line is invalid, it signals a condition up the stack. We attach the line encountered, in case we want to use it for error reporting.

Now imagine your parser is used in two situations: there is a compiler, and a REPL. For the compiler, you would like to abort at the first invalid line you encounter, which is what we are currently set up to do. But for the REPL, you would like to ignore the line and just continue with the next line.[^3]

To ignore a line, we would have to either do it on a low-level, return `nil` instead of signalling and filter out `nil` values up the stack. Handling the condition will not help us a lot, because at that point we have lost our position in the file already, or have we?

The next layer up is parsing a collection of lines:

```lisp
(defun parse-lines (lines)
  (loop for line in lines
        for entry = (restart-case
                     (parse-line line)
                     (skip-line () nil))
        when entry collect it))
```

This is where the magic begins. The `loop` construct just loops over the lines, applies `parse-line` to every element of the list, and returns a list containing all results which are not `nil`. The feature I am showcasing in this post is `restart-case`. Think of it this way: it does **not** handle a condition, but when the stack starts unwinding [^4] because we signalled a condition in `parse-line`, it registers a possible restart-position. If the condition is handled at some point,[^5] the signal handler can choose to restart at any restart-point that has been registered down the stack.

Now let us have a look at the callers:

```lisp
(defun parse-compile (lines)
  (handler-case
      (parse-lines lines)
    (invalid-line-error (e)
                        (print-error e))))

(defun parse-repl (lines)
  (handler-bind ((invalid-line-error
                  #'(lambda (e)
                      (invoke-restart 'skip-line))))
    (parse-lines lines)))
```

There is a lot to unpack here. The compiler code is using `handler-case`, which is comparable to `catch` in other languages. It unwinds the stack to the current point and runs the signal handling code, in this case `print-error`.

Because we do not actually want to unwind the stack all the way, but resume execution inside the `loop` in `parse-lines`, we use a different construct, `handler-bind`, which automatically handles `invalid-line-error` and invokes the `skip-line` restart. If you scroll up to `parse-lines` now, you will see that the restart clause says, if we restart here, just return `nil`, and `nil` will be filtered on the very next line by `when entry`.

The elegance here is the split of signal handling code, and decisions about which signal handling approach to take. You can register a lot of different `restart-case` statements throughout the stack, and let the caller decide if some signals are okay to ignore, without the caller having to have intricate knowledge of the lower-level code.[^6]

If you want to learn more about this, make sure to have a look at the book, it goes into much more detail than I did here.


[^1]: A "condition" in Common Lisp, as has been explained to me by [Michał "phoe" Herda](https://phoe.github.io), is a way of signalling arbitrary events up the stack to allow running of additional code, not just signalling errors. They're comparable to hooks in Emacs, but dynamically scoped to the current call stack.

[^2]: This is assuming of course that a line always represents a complete parsable entity, but this is only an example after all.

[^3]: I'm not saying that is necessarily a good idea, but it is something some REPLs do, for example some Clojure REPLs.

[^4]: Technically not unwinding yet, at least not in Common Lisp.

[^5]: If it isn't caught, you will get dropped into the debugger, which also gives you the option to restart.

[^6]: It does need to know about the registered `restart-case` statements though, at least by name.
