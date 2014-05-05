Title: Backup your work
Date: 2014-03-10 23:58
Author: sulami
Category: Coding
Tags: cron, dropbox, git, google drive, homeserver, raspberrypi, security
Slug: backup-your-work

When doing serious work involving code (or config files), backups are a
must-have. What you want to do are backups to (sort of) remote
locations, like separate harddrives, external drives or other
computers/servers, which is protecting your precious work from hardware
failure and to some degree (not so) accidental deletion.

You also want to be using a VCS, I will be using git, because it is
simple, fast and powerful. With git, you can combine version
controlling/branching and backups pretty easily. Go to your remote
location, make a directory *foobar.git*, init an empty git repo and add
it as remote to your project. Don't forget to push to it.

    cd ~/remote
    mkdir foobar.git
    cd foobar.git
    git init --bare
    cd ~/project
    git remote add backup ~/remote/foobar.git
    git push backup master

This is a minimal example of how to do this. You will want to exclude
large binary files like images, videos or binaries (duh..), as git can't
compare those and will compress each version. Instead, you probably want
to just compress and store them independently, version-wise or just one
set, depending on the type of files. Consider using tar, rsync and/or
cron for these backups.

In my case, I backup a simple video platform, backing up all the code
and configs using git and the video files, thumbnails and database in a
.tar.xz of which I only store the newest version. I store all my work
multiple times, 2 times on my workstation in case of hardware failure (I
don't use a RAID at the moment), once on a local server and once on an
external drive which is offline most of the time, so it even is
protected against malicious attacks.

I personally don't like Dropbox, Google Drive, Skydrive and all those
other clouds, because I prefer to keep my data on my own. Dropbox is
nice for syncing PDFs over different devices, but for everything
slightly important, I use git with a local repo and sometimes GitHub. In
case you now say, you can't afford your own in-house server, get a
Raspberry Pi Mod. B with a small SD-card, a power cable and some sort of
external drive, totalling maybe \$100. It uses near to no power, can run
24/7 without any noise or noticeble heat and use the external drive to
save all your stuff on a separate machine. It's that easy.
