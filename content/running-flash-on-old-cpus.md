Title: Running Flash on old CPUs
Date: 2013-05-29 11:26
Author: sulami
Category: Web
Tags: flash
Slug: running-flash-on-old-cpus

I recently had to refurbish an old laptop for my father which is now
about 10-12 years old. As Windows XP won't run anymore, I convinced him
to let me setup some similar Linux distro. I went for openSUSE 12.3 with
KDE, disabled compositing and it runs surprisingly smooth. Now he needed
to access some Flash-based web-apps and watching YouTube once in a while
would be nice aswell, but the Flash-plugin wouldn't load, neither 11.2
in Firefox nor Chrome, and 11.7 did not even show up anywhere. Firefox
threw the following error when started from a terminal:

    [Parent][RPCChannel] Error: Channel error: cannot send/recv

After a lot of research I found the problem. The CPU is an Athlon XP-M
2500+, which does not support SSE2 (only SSE), which is needed for "new"
versions of flash. My workaround is getting an old Flash package and
replace */usr/lib/flash-plugin/libflashplayer.so *with [the old
version][]. Aside from obvious security problems this runs well and
should do in most cases.

  [the old version]: /download/libflashplayer.so
    "libflashplayer.so"
