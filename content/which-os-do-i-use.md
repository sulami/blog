Title: Which OS do I use?
Date: 2015-06-10

Since I [wrote about systemd][0] a few weeks back, I have been looking out for
an operating system that I truly love, that is without systemd whatsoever. My
ideal specifications involved a philosophy that is close to BSD's, with a focus
on stable software and exhaustive documentation, while being reasonable modern
so that I do not run into incompability issues, and a couple of minor points
(like I prefer git over svn). After looking and testing for a while, going
through half the list on [without systemd][1], I found the [Manjaro OpenRC
spin][2].

Manjaro is an Arch-based distribution, that replicates the Arch repositories in
such a way, that there is a current branch that closely follows the Arch ones,
and a stable branch that is, well, more stable. At the moment I am running
Linux 3.18. It also uses pacman, which is one of the best binary package
managers I have ever used, and only clearly superseded by Gentoo's portage.

While you can use OpenRC, the in my eyes currently best init-system on Linux,
on Arch, doing so will result in major breakages, which is not exactly what
one would consider "stable". In the end, I have used enough cutting-edge
software and just want something that works with minimal maintenance. Manjaro
is just what I want. It is stable while not too old for me to run into any
issues, can use the AUR, has git-based repositories, and can run on
OpenRC/eudev/syslog-ng/lightdm/other software which has been replaced and/or
eaten by systemd.

The point of this post is merely to tell you, if you are interested in this
sort of thing, check it out. It is more practical that \*BSD or Gentoo, and
Void Linux has been a bit hit and miss in my personal experience.

  [0]: {filename}/systemdebacle.md
  [1]: http://without-systemd.org/
  [2]: http://sourceforge.net/projects/manjaro-openrc/

