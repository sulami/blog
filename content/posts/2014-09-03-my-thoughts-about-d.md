---
title: My Thoughts About D
---

Roughly two weeks ago, a blogpost about the [D language][d] popped up on both
hackernews and reddit. I took the time to read it, and it inspired me to try D
out for myself. Here are some of my thoughts, about two weeks into the
language, with prior experience in C but no real experience in C++ or
Objective-C. Scroll down for a TL;DR.

  [d]: http://dlang.org

I am assuming that you, the reader, are at least somewhat familiar with C, and
have some general knowledge about C++ and why it became one of the worst
languages there are, right next to PHP (actually for the same reasons). D aims
to be what C++ (and appearantly Objective-C) failed to be, a modern-times C
with proper classes, reasonable support for unittesting, garbage collection,
exceptions, faster compilation and much more.

The recommended way to use D is by using [dub][dub], a combined
package manager and build system, which reads the requirements of a programm
from a json, if needed downloads and compiles some modules, and then builds
your program. This is a great system, as long as there packages to use. Which
is the major downside of dub-based building. There are few packages in the dub
databases (321 at the time of writing), so chances are the libraries you need
are not available this way. But you can still use C libraries, because D is
*almost* a superset of C, which makes binding D to C really easy, almost
scriptable (actually a lot of the dub packages are just C-library bindings).
This is what rescues D in my eyes. Python is still one of the best userspace
language out there because of its versatility, and D can benefit from C's
versatility.

  [dub]: http://code.dlang.org/about

But let me give you a real first impression of D:

```d
import std.stdio;

int main(string args[])
{
    foreach (arg; args[1..args.length])
        writeln(arg);

    return 0;
}
```

This is an simple echo clone, which echoes back one command-line argument per
line. As you can see, it almost looks like C. Instead of `#include` there is
`import`, and there is native string support, as well as foreach, but the
general picture is pretty similar. But already the first line brings me to
something I really like about D, the standard library. [D's standard
library][std], called "Phobos" is incredible. Aside from the usual filesystem
inspection, math, time, hash and socket stuff, there is something like
[getopt][getopt], a library which allows you to make reading command line
arguments really easy, with defining short and long aliases (`-l|--list`) and
returning the not used arguments afterwards.

  [std]: http://dlang.org/library/index.html
  [getopt]: http://dlang.org/library/std/getopt.html

D has been modeled after C, which makes it both an application and a system
level language, but unlike most C-based languages, it also has a proper web
framework, [vibe.d][vibe]. Vibe is comparable to Flask, as in it provides a low
amount of predefined structures, and echo-webservers are just a couple of lines
long (as can be seen on the vibe website). But it can be used for large scale
projects, if properly maintained. Aside from native code made from D, which
makes it easily faster than most interpreted solutions, it uses the
[Diet][diet] template language, which is an extended Jade (used by node.js). I
would personally describe it as "the markdown of html templates", because it
almost looks like markdown, but with a lot more control over html attributes
and the ability to embed D code in the template. I am currently working on
proper syntax highlighting for diet in vim ([here][dvim]).

  [vibe]: http://vibed.org/
  [diet]: http://vibed.org/templates/diet
  [dvim]: https://github.com/sulami/diet.vim

If you now think "Wow, that sounds great", you are pretty much on my side, I
already fell in love with D. The only major downside I can see is the adoption.
D has been around for almost a decade now and the only case of someone actually
using it I have heard of (aside from myself and the D community) is Facebook. I
believe D could be the next big language, if it gets adopted.

**TL;DR** D is great for a lot of applications, especially ones with high
performance requirements, has a great standard library and relatively simple
interaction with C code, as well as a nice package manager/build system combo.
It lacks in terms of adoption, resulting in few ports to D, few frameworks, few
third-party documentation. If D gets adopted, it will be a major success.

