Title: Quickie: Fix Skype sound glitches on 64-bit Linux
Date: 2013-05-23 09:28
Author: sulami
Category: Linux
Tags: quickie, pulseaudio, skype
Slug: quickie-fix-skype-sound-glitches-on-64-bit-linux

After searching for fixed quite a long time, I finally found one that
actually fixed the occasional sound glitching in Skype with PulseAudio.
The solution is simple, open /etc/pulse/default.pa and change

    load-module module-udev-detect

to

    load-module module-udev-detect tsched=0

Just reboot afterwards and everything should be fine.
