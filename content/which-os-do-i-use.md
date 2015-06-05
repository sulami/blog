Title: Which OS do I use?
Date: 2015-06-05
Category: Linux
Tags:
status: draft

This is a very interesting question these days, as my last post was a bit of
a rant about systemd. Now that [most Linux distributions have adopted
systemd][0], it is getting increasingly difficult to build a useful system
without it.

I tried both [Gentoo][1] and [Funtoo][2], and really liked both Portage and
OpenRC, but with the majority of my desktop computing done on a laptop,
compiling this amount of software is barely an option. [Void Linux][3] has
its own init-system, runit, and its own package manager, xbps, and while
both are okay-ish, both failed to really impress me, especially xbps. I also
would like a more stable and less cutting-edge set of packages.

This seems to scream BSD, and while [FreeBSD][4] runs excellent on my
system, is stable, systemd-less and everything, it does not run a certain
proprietary piece of software that shall not be named here, but I am sadly
dependent on at the moment. So where does this leave me?

Today, I installed the [Manjaro OpenRC spin][5] on my machine. It works
very nice so far, everything works as well as on Arch, but just with OpenRC
and eudev (the udev-fork without the rest of systemd). This is by far easier
than ripping systemd out of Arch myself, which I have tried before. In
addition to that, I can use Manjaro's "stable" branch, which is better than
nothing.

