---
title: Using Hoogle for Your Project
---

[Hoogle](https://github.com/ndmitchell/hoogle) is a fantastic tool that uses
Haskell's type system to build a searchable index of types, functions and
constants, connecting with [Haddock](https://www.haskell.org/haddock/) to
provide documentation. It can be used to host browsable websites, like the
[Hackage instance](https://www.haskell.org/hoogle/) or the [Stackage
instance](http://www.stackage.org/lts-3.15/hoogle), or used for editor
integration, like in [vim](https://github.com/Twinside/vim-hoogle) or
[emacs](https://github.com/haskell/haskell-mode). Overall pretty great stuff.

But if working on your own project that is not online anywhere yet, setting up
a local Hoogle instance is a bit tricky. But not anymore. I wrote a relatively
simple shell script that sets up a local instance combining your project and
the Stackage snapshot your project is using. It currently is quite naive, and
assumes you are using Linux, and generally is a work in progress. But it works
on my machine™, so I am [releasing it to the general
public](https://github.com/sulami/dotfiles/blob/master/scripts/hoogle.sh) (the
link is to the master branch of my dotfiles, so it gets updated automatically).

Beware: There is no error checking whatsoever, so make sure you have GHC,
Stack, Haddock and Hoogle installed and in your `$PATH` (also curl and awk). It
might also eat all of your data. If you change your stack resolver, you need to
delete the Stackage Hoogle database yourself to trigger a download of the right
one, there is no version checking. Also local documentation paths do not work
when using `hoogle server`. Some of this might get fixed in the future.

