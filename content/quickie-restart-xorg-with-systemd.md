Title: Quickie: restart Xorg with systemd
Date: 2013-09-12 12:31
Author: sulami
Category: Linux
Tags: quickie, systemd, xorg
Slug: quickie-restart-xorg-with-systemd

As I am using KDE-Plasma on fglrx, I can only run my Xorg-session for
about two days before running into the unholy "Maximum number of clients
reached"-error.

    xlsclients | wc -l
    99

99 Clients should not be a problem at all, but fglrx sucks, so I need to
restart X. With systemd, this is pretty easy.

    systemctl isolate multi-user.target
    systemctl isolate graphical.target

This is comparable to *init 3* and *init 5*, but using systemd-targets.
