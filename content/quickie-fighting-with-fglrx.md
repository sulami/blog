Title: Quickie: fighting with fglrx
Date: 2013-07-09 22:12
Author: sulami
Category: Linux
Tags: quickie, fglrx, videocard, xorg
Slug: quickie-fighting-with-fglrx

I started playing games again, so I switched from radeon to fglrx, which
promptly broke my X11. In case you get stuck at "Reached target
graphical interface", switch to a different tty (alt+F1) and login. Then
look at the Xorg-logs at */var/log/Xorg.0.log*, which might say
something like

    (WW) fglrx: No matching device section for instance (stuff) found
    (EE) No devices detected

This means in your */etc/X11/xorg.conf* is no section for your
videocard, which is not to big of a problem. Log into root or use sudo
to run lspci and then edit xorg.conf and switch the PCI-bus-number to
the one your card is using. Reboot.
