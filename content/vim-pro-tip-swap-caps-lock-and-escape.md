Title: Vim pro-tip: swap caps-lock and escape
Date: 2013-05-14 19:16
Author: sulami
Category: Coding
Tags: vim
Slug: vim-pro-tip-swap-caps-lock-and-escape

Whoever uses vim a lot will have witnessed the difficulty to switch out
of insert-mode by using escape, mainly because it is far away from
everything else on your average keyboard. While vim already uses hjkl
instead of the arrow-keys, escape is a big let down. But we have a nice
alternative, caps-lock. No one really uses it, so why should we not just
swap those two? It looks like this:

    remove Lock = Caps_Lock
    keysym Escape = Caps_Lock
    keysym Caps_Lock = Escape
    add Lock = Caps_Lock

This is a simple script to open with xmodmap (Arch package:
[xorg-xmodmap][]). As it is only session-persistent, add it to .xinitrc
or equal startup scripts.

  [xorg-xmodmap]: https://www.archlinux.org/packages/extra/x86_64/xorg-xmodmap/
    "xmodmap"
