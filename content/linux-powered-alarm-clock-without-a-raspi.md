Title: Linux-powered alarm clock without a RasPi
Date: 2013-06-11 11:39
Author: sulami
Category: Linux
Tags: cron, mplayer, onlineradio, rtcwake
Slug: linux-powered-alarm-clock-without-a-raspi

My method of using the Raspberry Pi as alarm clock with mplayer-cronjobs
is also usable on regular desktop machines. To overcome the wasted power
and noise while sleeping, the computer is going to sleep as well,
suspending to either ram, disk or both. Then you can use *rtcwake* to
let the hardware clock wake it up automatically just a minute before
alarm time.

    sudo rtcwake -m no -l -t $(date +%s =d'tomorrow 07:30')

What this does is it prepares the hardware clock to wake, but does not
initiate the sleep yet, so you can continue using the computer and bring
it down later, before going to sleep. It is also told that the hardware
clock is using local time (*-l*) and not UTC (*-u*) and is given a
unix-timestamp by *date as* the alarm time. For optimal results, have
your PC wake up a minute before you, so the cronjob does not get cut
off.
