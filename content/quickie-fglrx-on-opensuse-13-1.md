Title: Quickie: fglrx on openSUSE 13.1
Date: 2013-11-22 14:24
Author: sulami
Category: Linux
Tags: quickie, fglrx, opensuse, updates, xorg, yast
Slug: quickie-fglrx-on-opensuse-13-1

Some days ago, openSUSE 13.1 was officially released. The new version
seems to be pretty nice, but, as usually, breaks fglrx. The temporary
fix is quite easy:

Open YaST and navigate to your software repositories. Edit your
fglrx-repo and change the domain to
*http://geeko.ioda.net/mirror/amd-fglrx-beta/openSUSE\_13.1/* . Now you
should be able to remove fglrx for 12.3 and install fglrx beta for 13.1.
In a first test, the beta driver seems to work pretty well.
