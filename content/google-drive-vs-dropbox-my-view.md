Title: Google Drive vs Dropbox - my view
Date: 2013-06-04 15:48
Author: sulami
Category: Web
Tags: dropbox, google drive
Slug: google-drive-vs-dropbox-my-view

I used to use Dropbox some years ago when it came up but somehow lost
track of it when reinstalling and just stopped using it. Some time
later, I wanted to sync some stuff up between my multiple OS's, PC's and
my phone, and being the Google-fanboy I was I used Google Drive. When I
switched to Arch Linux as my only OS last year, I found [grive][], an
open-source Drive client which works from a shell, well, most of the
time. Now, on openSUSE, I had some problems finding a proper client, so
I rediscovered Dropbox. Now in short, pros and cons of each.

###### Google Drive:

-   Lots of space for free (15G right now)
-   Clients for Windows, Mac, Android, iOS and community-made Linux
-   Integration with Google Docs
-   Probably high reliability/security (it's Google, these guys are the
    internet)

###### Dropbox:

-   Just 2G for free (still enough for me)
-   Clients for virtually everything, works better on Linux
-   LAN-sync
-   Not as reliable (down one day last week) or secure (there have been
    leaks)

Grive on Linux is not a daemon, so you should consider running it via
cron every few minutes, Dropbox works just like on Windows and syncs on
changes. Because of the better client I tend to prefer Dropbox over
Google Drive, when Google comes up with a proper client for Linux, this
is likely to change.

  [grive]: https://aur.archlinux.org/packages/grive/ "AUR"
