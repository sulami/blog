Title: SystemDebacle
Date: 2015-04-28

I am writing about systemd today, because now, that the new release of Debian
with systemd is out, there is pretty much only one major distribution left that
is not forcing its users to use systemd, that being Gentoo. I have for quite a
long time defended systemd while it has been the target of extensive criticism
almost since its inception. I will mostly be writing about my concerns about
the \*nix ecosystem from the standpoint of an user and programmer. Today, most
of my machines are running either Gentoo or some BSD, mostly OpenBSD.

Here are some facts:

- systemd only runs on Linux, because of Linux-specific kernel features it uses
- systemd is combining lots of subsystems to work together
    - For example the init system and the syslog for service-specific logs
    - The extend of the subsystems it manages is set at build time
    - Some might call this feature-creeping
- systemd is the de-facto standard in the Linux world as of today
    - All major distributions except Gentoo ship it by default without the
      option to use another init system
- You cannot remove systemd from your distribution and expect it to work at all
    - That is except on Gentoo, where systemd is just an option and OpenRC the
      default
    - This means, in conjunction with the previous point, most people on Linux
      are stuck with it

What we are seeing here is bundling, a commong strategy used by commercial
companies to ensure their products cannot be replaced easily. Think Internet
Explorer. People running Linux, especially the ones recently switching from
other operating systems are forced to use it. They are impressed by its ease of
use (which admittedly exists). But there is no way for competition to arise,
which is just as important in the open-source ecosystem as in the commercial
space. Imagine all the lost innovation because an outright better
init-system/syslogger/whatever systemd swallowed cannot gain any traction
without users.

I believe this is a very dangerous path we are walking.

