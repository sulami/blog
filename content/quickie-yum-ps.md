Title: Quickie: Yum PS
Date: 2014-02-18 20:16
Author: sulami
Category: Linux
Tags: quickie, security, updates, yum
Slug: quickie-yum-ps

Coming from SUSE I am now primarily using RHEL/CentOS/Fedora-type Linux
distros, which means the package manager of my choice is yum. While yum
is quite a bit faster than zypper in almost every situation, it does not
provide the really useful function of showing you which running
processes are affected by package changes, which is particularly useful
on servers which aren't rebooted every few days (my homeserver is now
running for \~160 days without reboot).

But there is a simple helper: get *yum-plugin-ps*, a plugin which, who
would have guessed it, adds the functionality described into yum. Now
you can run *yum ps* and get a list of running processes, ordered by
changed files they use, so you can reload/restart those services and
hopefully prevent bad guys from exploiting already patched security
holes.
