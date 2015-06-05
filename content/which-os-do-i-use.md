Title: Which OS do I use?
Date: 2015-06-05
Category: Linux

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

But I feel that I still have to explain why I am going through all this just
to avoid systemd. Since systemd started feature creeping, so since it went
beyond simple init and assimilated the syslogging (which I can sort of
understand, since it allows for more intelligent logs), network and firewall
management, login management, bootloading, DNS, device management via
udev and who knows what else, it slowly chips away the modularity of a
system. There even is [a website dedicated to abolishing it][6]. One of the
points often made are the binary logs. While I agree that I would prefer
simple plain text logfiles, just for forensic sake, and being able to use my
favourite tools to work with them, I do not really care all that much about
the issue.

My main concern, now that it is this widely adopted, and the systemd
developers, most prominently Poettering and Sievers, are known for not caring
all that much about the rest of the world, diplomaticly speaking, is that
systemd is getting mandatory for an enduser-reacdy Linux system, effectively
killing any choice.

\*nix has always been about choice, and the modularity of the whole system
always supported that. Ideally, you can choose your bootloader, your
init-system, your desktop environment and all the other programs individually
to build excactly the system you need. But both Gnome and KDE need systemd
these days. Almost all of the big distributions come with systemd as only
supported init-system, and all the packages and default configs are meant to
use it. Converting a systemd-system to something else always results in major
breakage.

And systemd makes more and more of these "modules" mandatory, while Lennart
Poettering still claims, [systemd were modular][7]. It might be, if you intend
on running [LFS][8].

  [0]: {filename}/systemdebacle.md
  [1]: https://gentoo.org/
  [2]: http://funtoo.org/
  [3]: http://voidlinux.eu/
  [4]: https://freebsd.org/
  [5]: http://sourceforge.net/projects/manjaro-openrc/
  [6]: http://without-systemd.org/
  [7]: http://0pointer.de/blog/projects/the-biggest-myths.html
  [8]: http://www.linuxfromscratch.org/

