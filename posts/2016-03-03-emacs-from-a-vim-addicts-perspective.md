---
title: Emacs From a Vim Addict's Perspective
---

I have been playing around in Emacs lately. Vim has been the core of my workflow
for half a decade now, and I just cannot imagine going back to non-modal editing
anymore. I will now explain, why I might just switch to emacs in the long-term.

If you compare Emacs and vim, you will see that vim's modal philosophy is the
clearly superiour one, but you will also find that vim is somewhat limited. It
works quite well out of the box, and my current `.vimrc` is about 300 lines in
size, most of that just setting options, defining shortcuts and loading plugins.

VimL tends to work for these things, but as soon as you start to change default
behaviour and writing your own functions, it all falls apart. Vim does not know
anything about its own state, there is no proper programmatical way to query it,
and VimL as a language is extremely clunky and difficult to debug once you
surpass simpe `set` statements. This shows for example in the built-in help,
that consists of plain text files that display the default configuration.

Emacs on the other hand is pretty much just a Lisp interpreter with a simple
text editor written in said Lisp running on top of it. Because the configuration
is done on the same level that the editor itself is written on, in the same
language, you can essentially completely replace all default behaviour, and
emacs has a great framework to get the current state and configuration of the
editor, because everything is just saved into variables. There is this funny
quote about Emacs being a great operating system lacking a decent editor, and I
find that it is pretty accurate.

Enter [Spacemacs][sm]. Spacemacs is an Emacs distribution, meaning it is a
prepackaged set of packages and configs that are supposed to more or less
implement vim's behaviour on the emacs platform and integrate well without
having to write several thousand lines of Elisp yourself, which is what you
might actually need, depending on how much of vim you want in Emacs.

Spacemacs got reccomended to me first by [Kritzcreek][kritz] some time ago, and
ever since I have been keeping an eye on it. I generally like to build my own
configs and only stealing small parts from other people, but with default emacs
being so different from the modal vim-like experience I want, it would take
months to even come close to my vim setup. Spacemacs on the other hand comes
with mostly sane defaults, and my overall custom config on top of its defaults
is currently less that 100 lines of Elisp. I also want to mention
[Aaron Bieber][bieber]'s [talk about evil-mode][talk] for finally pushing me
over the line and investing a weekend into learning enough emacs to be
productive.

I am currently still in the progress of building my own emacs config from
scratch, recreating the parts of Spacemacs I actually use, but I am not sure if
I will ever get it to a point where it is even remotely as polished. Another
factor of course is that I do not really want to waste too much time on this,
and at least right now, Spacemacs does what I need it to do. In the end, you
always have to think about if that extra bit of more custom behaviour is worth
the time-investment to get it to work.

  [sm]: http://spacemacs.org/
  [kritz]: https://kritzcreek.github.io/
  [bieber]: http://blog.aaronbieber.com/
  [talk]: https://www.youtube.com/watch?v=JWD1Fpdd4Pc
