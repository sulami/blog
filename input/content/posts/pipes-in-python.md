title = "Pipes in Python"
timestamp = "2018-08-21"
tags = ["python"]
---
I just found [an article about pipes in Python](https://hackernoon.com/adding-a-pipe-operator-to-python-19a3aa295642) on lobste.rs and was reminded that I was toying with the exact same thing recently. Using a lot of functional languages (mainly Haskell, Clojure, Elixir)[^1] and also a fair bit of bash, I am very used to streaming data through chains of functions using pipe-like constructs. Python does make this quite difficult and encourages a more imperative approach with intermediate variables.

The author of the article above uses AST rewriting, which I have to admit is very clever, though hard to introspect and extend unless you are already familiar with AST manipulation in Python.

My approach is slightly different, and yields a less pretty syntax, but is arguably more flexible and extensible, by using a plain function. The definition currently looks like this:

```python
def pype(x, *fs):
    """
    Pipe function. Takes an initial value and any number of functions/methods.
    Methods as strings. Additional args are supported for functions & methods
    by suppling a step as a tuple/list with function/method as the first
    element and the args as the rest. The pipe input is used as the last
    argument in this case. Currently no kwargs.
    """
    while fs:
        f = fs[0]
        args = []
        if isinstance(f, (list, tuple)):
            args = list(f[1:])
            f = f[0]
        if isinstance(f, str):
            if f.startswith('.'):
                x = getattr(x, f[1:])(*args)
            else:
                x = x[f]
        elif isinstance(f, int):
            x = x[f]
        else:
            x = f(*args + [x])
        fs = fs[1:]
    return x
```

The docstring is a bit abstract, so I think an example is much more explanatory:

```python
from pype import pype

def add_suffix(number, s):
    return '{} is {} cool!'.format(
        s,
        ' '.join('very' for _ in range(number))
    )

pype(
    '   abc: {}   ',
    '.strip',
    ('.format', 3),
    (add_suffix, 2),
    '.upper',
)

# 'ABC: 3 IS VERY VERY COOL!'
```

I am aware this is a very constructed example, but you get the idea. It currently does not handle keyword arguments, and you cannot specify in which place you would like the input argument to go, it always takes the last slot, which isn't always convenient, but this way you can avoid using =lambda=s everywhere, which are quite long in Python[^2].

I might extend this further in the future and maybe introduce it into some code bases of mine, if it turns out to be useful.


[^1]: &#x2026; but being paid to write (mostly highly object-oriented) Python four out of five days a week. We do use some Elixir at my place, but we are still a Python shop first.

[^2]: I quite enjoy Elixir's solution: `&func(1, &1, 3)`, `&1` being the placeholder.
