Title: Quickie: fix Windows' stupid quotation mark behaviour
Date: 2013-12-29 22:24
Author: sulami
Category: Coding
Tags: quickie, keyboard, typing, vim
Slug: quickie-fix-windows-stupid-quotation-mark-behaviour

Two days ago, my system-SSD died, which happens. Because of this
incident, I lost both my Arch Linux and my Windows 7 installation. While
I wait for a new SSD to arrive, I installed some Win 7 Professional I
found. And while I am German and use the German language for my
operating systems, I prefer qwerty-keyboards over the German qwertz
ones. So I installed Win 7 using the "US - International" layout, which
seemed to work just fine. But it does not.

When typing quotation marks like " or ', you have to press another key
before it actually gets print, which is annoying while coding, but even
worse for using vim (think ci" or similar). The fix is quite easy:
change the default layout to just "US", and hitting quotation marks
instantly prints quotation marks.
