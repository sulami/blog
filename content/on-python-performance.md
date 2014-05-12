Title: On Python performance
Date: 2014-05-12 02:15
Category: Coding
Tags: python, c

I recently wrote [Conway's Game of
Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) in both
performance optimized Python 3.3 and C99, compiled with clang, without
optimizations. I usually say to people that Python is quite fast when properly
optimized, and amongst interpreted languages this is true. But after this
experiment, I understand how big the difference can be und the right
conditions. Here are the runtimes of my Games of Life, generating a 120x40
screen and printing it out for 5000 cycles:

Python 3.3, fairly optimized: 2:05,73 minutes

C, not optimized: 9,413 seconds

This is a pretty big difference, ~1330%. This should explain, why, besides the
stellar portability, Kernels and generally all performance critical software is
written in some form of C.

Huge parts of this are actually printing the screen, looks like Python's print
is quite slow. Without printing it, generating 5000 cycles takes 32,244
seconds, which is still more than three times as much as C needs with printing.

For most operations, Python's speed is more than needed, but sometimes you just
need that extra bit of speed, and this is why you can [use C from within
Python](https://docs.python.org/3.4/library/ctypes.html).

