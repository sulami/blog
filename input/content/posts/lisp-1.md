title = "Lisp-1 Has Won"
timestamp = "2020-06-10"
tags = ["lisp"]
---
I am currently working on a compiler for a new programming language which has been in the making for a few months at this point. There is nothing public to show yet, everything is very early stage, and there are plenty of decisions to make and work to be done before I will publish anything.

That being said, I will write about both the progress as well as different topics I come across, so [stay tuned](file:///atom.xml) if you are interested in that.

The language I am writing currently has a Lisp-like syntax, because that is easy to parse an work with,[^1] which is why I am sharing some thoughts on one of the big bike sheds in software history.


# LISP<sub>what?</sub>

LISP<sub>1</sub> and LISP<sub>2</sub> are terms to describe the way symbol namespaces work in different LISP-like programming languages.[^2] The explanation is actually very simple, LISP<sub>1</sub> has a single shared namespace for functions and variables. This means a symbol can refer to either a function or a variable, but not both. Consider the following Racket code:

```scheme
(define (double x)
  (* 2 x))

(define triple (* 3 4))

double
;; => #<procedure:double>

triple
;; => 12

(double triple)
;; => 24
```

When you resolve a symbol to a variable, you cannot know if it will resolve to a function or not.

LISP<sub>2</sub> on the other hand has a separate namespace for functions. This has the advantage that every name can be used twice,[^3] once for a function, and once for a variable. The tradeoff is that the user has to specify in which namespace they want to resolve a symbol. Consider the following Emacs Lisp code:

```emacs-lisp
(defun double (x)
  (* 2 x))

(defvar double (* 2 4))

(funcall #'double double)
;; => (funcall <function double> <variable double>)
;; => (double 6)
;; => 12
```

Note the added punctuation to denote the first `double` as a symbol resolving to a function.


# LISP<sub>why?</sub>

LISP is one of the oldest programming languages that is still used commercially today in some form, if you accept Common Lisp in its lineage. It appears that the namespace separation in the original LISP 1.5 was mostly incidental, and [has been regretted since](http://www.nhplace.com/kent/Papers/Technical-Issues.html).

The set of LISP<sub>2</sub> languages is quite small these days. Besides Common Lisp and Emacs Lisp, both of which are over three decades old at this point, there are also Ruby and Perl.[^4]

The other ancient LISP-like language, Scheme, is a LISP<sub>1</sub>, and so is its popular modern dialect Racket (as demonstrated above). Almost every other somewhat popular language chooses to share a single namespace between functions and variables. Examples include Clojure, Janet, Python, Java, JavaScript, and even Rust.

Clearly the benefits of less syntactic clutter and cognitive overhead have won in the popular arena, to the point that the established de facto standard itself becomes a good reason to stick with a single unified namespace. Of course improvement, by its very definition, always requires change, but language designers need to be acutely aware of the cost incurred by diverging from the established norm.


[^1]: I just really don't want to deal with operator precedence, and honestly I like writing Lisp, so I find it unlikely that I'll actually change to a non-Lisp-like syntax.

[^2]: It can also be applied to other languages with first-class functions, e.g. Python & Ruby.

[^3]: I'm glossing over the fact that Common Lisp actually has more than two namespaces, depending on your definition of the term namespace.

[^4]: Honourable mention: [Lisp Flavoured Erlang](http://lfe.io/) is a LISP<sub>2</sub>.
