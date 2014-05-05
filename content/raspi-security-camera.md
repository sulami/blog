Title: RasPi: security camera
Date: 2013-05-24 09:56
Author: sulami
Category: Linux
Tags: raspberrypi, vlc
Slug: raspi-security-camera

Today I dug out my 2011 Raspberry Pi Model B (the first edition with
256M ram) and reimaged my SD-card. This time I wanted to try out
[Raspian][], after only using ArchARM before. WOrked out pretty well, I
am somewhat lucky because the SSH-server is enabled by default, because
I happen do not own a HDMI-monitor. My first idea was to use a really
old webcam (VGA, nearly 10 years old, no Windows drivers existent
anymore) and stream it to my network. The more or less intelligent
choice was to use vlc for capturing and encoding, which made it quite
easy.

First, we install vlc on our Pi, I am running headless (without X) via
SSH:

    sudo apt-get update
    sudo apt-get install vlc

Then we connect the webcam and start our stream:

    clvc v4l2:///dev/video0 --sout '#standard{access=http,mux=ogg,dst=192.168.178.44:8080}'

I used HTTP-streaming because it is pretty straight forward and okay for
my hobby project, UDP multicasts are probably better. The destination-IP
is the internal IP of the Pi. Then I open vlc on my machine and open a
network stream (ctrl-n), http://192.168.178.44:8080 and voila:

![picam][]

  [Raspian]: http://www.raspberrypi.org/downloads "RasPi"
  [picam]: /images/picam-1024x435.png

