Title: My own perfect world (aka what you should do)
Date: 2014-03-17 23:59
Author: sulami
Category: Web
Tags: firefox, flash, git, homeserver, irc, privacy, security, skype, vim, vlc, weechat
Slug: my-own-perfect-world-aka-what-you-should-do

I have to admit, I am a little bit perfectionist. Maybe more. There are
some conventions I found useful, but there are always people who do not
follow those conventions, some because they do not want to, but mostly
they just do not care. I just want to list some (read: non-exhaustive
list) of these conventions and maybe help you become a better human. In
my eyes. Which are the only relevant eyes.

General Stuff
=============

#### Encoding

UTF-8.

There is close to nothing in the western world which does not work with
unicode, but many thing that will not work without it.

#### Websites

You own a website? Great. Here are some things to not do:

-   overly heavy use of javascript/ajax
-   using POST where GET is usable (POST breaks bookmarks)
-   using GET where POST is required (login information, ...)
-   storing cleartext passwords (salt and hash them, only compare
    hashes)
-   storing any kind of userdata you do not need (hackers/leaks)
-   using flash for something that is neither a video nor a game
-   use a popup to ask me to like your facebook-page (or some other
    social bullshit)
-   split your content way to often to generate additional pageviews
-   anything with sound which is not the main point of the site

#### Passwords

Use unique, randomly generated passwords for each service, store them in
some sort of encrypted file (like KeePass). If you prefer to know your
passwords, choose a scheme to generate passwords for a given service, so
you do not use the same password everywhere. Also, long passwords are
better than short ones, special characters are mostly good and no one
who knows you should be able to guess your passwords, so no names of
SOs, children, parents, BFFs, ...

#### Backups

Do regular backups of everything you own. External harddrives, CDs,
Homservers, whatever. Just no "clouds", you cannot rely on those both in
terms of security and reliability. Always have at least two copies fully
under your control. And maybe read [my post about using git to backup
your work][].

#### Prefer Open Source

Not (only) for philosophical reasons, but for security reasons. If you
use a reasonably large open sourced piece of software, even if you are
not able to write a single word of code, most assuredly some fanatic has
already checked the whole code for security holes. In proprietary
software, sometimes only less than ten people have access to the source
code, and everything they do not catch is a potential way for you to
loose your access to let's say your bank account. Would suck, would it
not?

Mail
====

Mail is a big part of the internet and my life as well. But there are
some things that really bug me when reading mail.

#### Do not use HTML-mail

There is no reason at all to use HTML in mails, except you send
professional advertisements or something like this. But for simple mail,
just containing text and maybe some attachments, why would you use HTML?
To make it look prettier? I open my mail in mutt (read: a text console),
so I get to read around all your fancy HTML-tags.

#### Do not write in single/ridiculously long lines

This is a big Outlook/Live Mail/whatever it is called right now-issue.
Even though you see the linebreaks at the end of your writing area,
there are actually none, Outlook breaks it "live". When using proper
text-only-mail, this is bad, not like really bad, but somewhat bad. I
usually correct those, and by "I" I mean vim does it for me, but still.
I personally prefer breaks at a maximum of 72 characters per line, which
is a commonly used standard.

#### Do not TOFU

No, this one is not about your eating habits, TOFU stands for "Text Over
Fullquote Under". This is what Outlook (sic) does per default.
Fullquoting is useful in many situations and does not introfuce to much
overhead when using text-only mail (my mails very rarely exceed 10KB).
But instead of writing your answer over the mail you received, place it
below. This way, we both can read the coversation from top to bottom
while writing our mails.

Coding
======

If you and I are working together on a software project, there are some
things I greatly appreciate.

#### Use a (proper) coding style

Coding styles are everything when coding cooperatively. My favourite
coding style is the Python one ([PEP8][]/[PEP7][]), which means for most
languages:

-   no tabs
-   4 space indentation
-   prefered max line length: 80 characters

Indentation can be tabs alternatively, but **have to** be standardized.
In some editors, tabs are 4 spaces long, in others 8, and in some 6.
This makes code indentation a mess and ruins the readability. The line
length limit is for sane editing, 100 characters are also acceptable,
anything more than that is probably poorly readable in most editors and
screams "bad code".

#### Use a VCS/SCM

I could have also said "use git". But I see that some people prefer
mercurial or SVN (why...?), that is okay, too. Even when working alone,
always use some sort version control software, or you will break your
code one day and spend hours tracking down the bad change. Diffs and
rollbacks are vital for efficient coding.

#### As you are using git, branch

As git is the best vcs, in my opinion, you are using branches, aren't
you? Never, and I mean never, work on master. Master is for tested code
only. Always work on "wip", "robin" or "fix". It does not matter how you
name your branch. If you want to change something, branch, make your
change, test it(!), and merge back into master if your change breaks
nothing. This is even more important when coding cooperatively, as I do
not want to pull your work in progress on master, branch to my work in
progress and wonder why it is broken. Which brings us to

#### Work locally

Many devs are used to working directly on a server, and sometimes this
is the only way to effectively test their changes, as they are missing
the needed environment for the code. This is okay. But as you are still
using git, always try to clone a personal copy of the code to work on,
and only push back your branch(es) and possible changes to master to
save your work centralized. Otherwise there might be a hell of
simultaneously edited files and lock-/swap-/backup-files.

Software Recommendations
========================

I have used so much software over the years, I found my favourite
software for most of my needs, and I compared a lot. My favourites are
always the first ones.

-   OS: Linux (Fedora/OpenSUSE/Arch) or Windows 7 x64 on desktops,
    RHEL/CentOS/OpenSUSE/\*BSD on servers
-   Password Management: KeePass (version 1, version 2 is not widely
    available yet)
-   Text editing/coding: vim, Kate, Notepad++ (on Windows)
-   VCS: git
-   Mail: mutt, Thunderbird
-   VoIP: Skype (still searching for a better alternative, not secure in
    any way), Mumble
-   IRC: irssi, weechat, Quassel
-   VMs: Virt-Manager/QEMU with KVM, Xen (for PVM on hosts without
    CPU-extensions), VirtualBox (on Windows)
-   Video: VLC
-   Music: ncmpcpp+mpd, Rythmbox, foobar2000 (on Windows)
-   Office: LaTeX for papers, LibreOffice
-   Web: Firefox
-   Webserver: nginx
-   Filesharing: NFS, Samba (FTP is old and cluttered)
-   CMS: Django (anything), Wordpress (blogs), Drupal
-   Programming languages: Python (platform independent, modular, nice
    to write and read), C(++) (faster, for performance-critical
    applications)

  [my post about using git to backup your work]: /backup-your-work.html
    "Backup your work"
  [PEP8]: http://legacy.python.org/dev/peps/pep-0008/ "Python.org"
  [PEP7]: http://legacy.python.org/dev/peps/pep-0007/ "Python.org"
