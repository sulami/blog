---
title: FrankenWM
---

[FrankenWM][fwm] is a small, dynamic tiling window manager for Xorg. It is
written purely in C and configured using a header at compile-time. It has been
forked from monsterwm ages ago, and while the configuration and usage still
feels a lot like monsterwm, the underlying code is almost a complete rewrite.
It is also used on the asynchronous XCB library as opposed to the synchronous
Xlib, which monsterwm is based on, which makes it even faster.

Here is a quick overview of the features it brings:

- Tons of layouts, currently 13
- Gaps and borders that are configurable at runtime and optionally per desktop
- Proper minimizing using a desktop-specific client-stacks
- Real support for floating windows, for mouse-less desktops
- An optional scratchpad window that can be shown and hidden quickly from
  anywhere
- Configurable like almost nothing else, even with regexes to control window
  spawning
- Prepared for a panel of your choice by leaving configurable free space on the
  screen and outputting information to display on it
- And probably more which I forgot

Even other people like it (actual quotes):

```
sulami dude you made frankenwm
holy cow i love your work
```

*~ dsplayer14*

```
i could never really get into tiling window managers
im sure they make people efficient as fuck, but i like my dynamic wm
i spent way too much time configuring them, anyways. the only one i enjoy using is frankenwm
```

*~ adam*

```
great documentation, a fully commented config file + great man page got me up and running in a matter of minutes.
```

*~ t1ngol*

```
Oh dear lord, I love the guy who developed this WM. Such lovely documentation.
```

*~ pateoigras*

Get the source [here][fwm]. Pull / feature requests welcome, bug reports as
well.

  [header]: /images/frankenwm.png
  [fwm]: https://github.com/sulami/frankenwm

