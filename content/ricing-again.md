Title: Ricing again
Date: 2014-06-15 21:15
Category: Linux
Tags: monsterwm

I have to confess, I have been ricing again. Today was a slow sunday, no real
actual work, but I had an Arch VM I had setup to test some kernels sitting here
without X. While I have been rather happy with Fedora 20 and Gnome 3, which
feels like a real DE again and lets me focus on getting actual stuff done while
looking sort of pretty (although tweaking it is a pain), I often miss the
easier time of running Arch/i3 as main distro. I3 is a technically superb WM,
perhaps the best I have encountered thus far, but I dislike the manual tiling
philosophy. I also like bspwm, but the b-tree tiling is not useful very often,
for my day-to-day use I prefer v-stack/b-stack/monocle like in dwm. Some time
ago, I tried monsterwm, which is a slimmer dwm and instantly fell in love. So
my Arch VM is now running
[monsterwm](https://github.com/c00kiemon5ter/monsterwm) and
[bar](https://github.com/LemonBoy/bar) with xcompmgr for dropshadows. I applied
the *uselessgaps* patch to monsterwm which does the same thing like on dwm,
adding some pixels inbetween the windows so you can see parts of the wallpaper.
The result looks somewhat like this (beware, Qemu bugs the dropshadows, they
are all over the place):

![scrot](http://i.imgur.com/8TCX2Qg.png)

![scrot](http://i.imgur.com/sX2vYJ6.png)

Ignore the dropshadows, on a real machine they would look like they should, but
in my VM they do not update properly and never have in the past. The wallpaper
is just a random one I pulled from my harddrive. All the configs can be found
in my [git repo](https://github.com/sulami/dotfiles/monsterwm).

