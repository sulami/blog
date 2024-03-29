title = "Raspberry Pi - Radio-Alarm"
slug = "raspberry-pi-radio-alarm"
timestamp = "2013-06-07"
---
Some years ago I had the idea to use online radios as flexible alarm clocks, but the main problem was having to run the computer all night long, which results in high power consumption and noise. While netbooks can counter this to a certain degree, the Raspberry Pi is the optimal way of achieving our goal. You will need:

-   Raspberry Pi model B (+ power cable, SD-card with OS)
-   speakers, preferably 3.5mm jack
-   network/internet access
-   control access directly or via ssh

My RasPi is running [Raspian](http://www.raspberrypi.org/downloads) right now, but any Linux should do. You connect everything and boot it up, then access a console. We need to install mplayer, which will play our stream. If needed, update the package lists before installing.

```sh
# Raspian:
sudo apt-get install mplayer

# ArchARM (as root):
pacman -Sy mplayer

# Pidora:
sudo yum install mplayer

# openSUSE:
sudo zypper in mplayer
```

Next thing to do is writing a short playlist. The idea is to have a fallback if the internet connection or your radio station goes down, so you can switch to a local file to play instead. Use any text editor you want (vi, vim, emacs, nano, or graphical ones) to write something like my *playlist.pls*:

    [playlist]
    File1=http://mp3.ht-stream.net
    Title1=HouseTime
    Lenght1=-1
    File2=/home/pi/alarm.mp3
    Title2=FallbackAlarm
    Length2=-1
    NumberOfEntries=2
    Version=1

Mplayer will try to play File1, my stream, and if it fails, continue to play File2, the fallback. You can also add more stations and files and use the `-shuffle` parameter later on to play random music.

Now, ensure that cron is running (`ps aux | grep cron` should give you a hint) and type `crontab -e` to enter your cronjob (preferably as a non-root user). Depending on your OS there will be some comments you can ignore. Add a entry like the following:

    30 6 * * * env DISPLAY=:0.0 /usr/bin/mplayer -playlist /home/pi/playlist.pls > /dev/null

This will fire up mplayer everyday at 6:30 in the morning using the provided playlist and then deletes the text-output. Using complete paths is crucial here, cron does not use the $PATH variable. If you read up on cron, you can setup complete sets of alarms, depending on days of the week and more. The env setting might be needed on some systems which won't play and istead send you something like "connection by pulse refused" per system-mail. Save the file to install the cronjob.

When waking up, you need to kill mplayer, as it is running in the background. The easiest way of doing so is via `killall mplayer`, which is not the finest way of doing things but should do the job.
