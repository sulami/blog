Title: Install Cyanogenmod from Linux
Date: 2013-07-01 16:56
Author: sulami
Category: Linux
Tags: cyanogenmod, phone
Slug: install-cyanogenmod-from-linux

This is what I did today, I installed CM10 on my Sony Xperia Pro, which
was running CM7 before. There are a lot of guides around the net about
how to do this, but understandably most of them are written for Windows.
On openSUSE, flashing your rom is probably even easier than on Windows,
all you need is *android-tools* from the official repos.

    sudo zypper install android-tools

Then you get your rom zip (Xperia phones [here][]), extract it to your
phones sd-card and follow the [instructions][]. When it comes to
flashing the CM kernel, first extract it on your PC, start a terminal
and type the fastboot-command. THEN plug in your phone while holding the
menu (or volup, depending on device) key. It should look something like
this:

    ~/somefolder > % fastboot flash boot boot.img
    < waiting for device >
    sending 'boot' (6656 KB)...
    (bootloader) USB download speed was 9185kB/s
    OKAY [ 0.750s]
    writing 'boot'...
    (bootloader) Download buffer format: boot IMG
    (bootloader) Flash of partition 'boot' requested
    (bootloader) S1 partID 0x00000003, block 0x00000280-0x000002e3
    (bootloader) Erase operation complete, 0 bad blocks encountered
    (bootloader) Flashing...
    (bootloader) Flash operation complete
    OKAY [ 1.332s]
    finished. total time: 2.083s

The rest should be like in all other guides, pretty simple.

  [here]: http://freexperiaproject.com/ "FXP"
  [instructions]: https://sites.google.com/site/projectfreexperia/download/howto-install
    "FXP"
