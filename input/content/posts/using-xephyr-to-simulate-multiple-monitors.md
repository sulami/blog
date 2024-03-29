title = "Using Xephyr to Simulate Multiple Monitors"
timestamp = "2014-12-19"
---
One of the most requested features in [FrankenWM](https://github.com/sulami/frankenwm) is support for multiple monitors. I currently only use one monitor, mostly because I do not need more right now (there have been time where I have been using 4 monitors at the same time, all connected to one machine). This situation makes it quite difficult for me to develop such a rather big feature, simply because I cannot test it at all. I have tried to figure out how to use nested X servers to simulate multiple monitors on a single screen for a few weeks now, finally finding the solution today.

The nesting tool of my choice is [Xephyr](http://www.freedesktop.org/wiki/Software/Xephyr/), because it is quite feature-rich and easy to use (in comparison to Xnest). I have also experimented with Xvfb and Xdmx, the latter being unable to use randr, which is a dealbraker. But while scraping mailing lists today I found the magical solution, and it only needs Xephyr.

```sh
Xephyr -screen 640x480 -screen 640x480 +xinerama :1 &
```

This simple line starts Xephyr with two screens in one and uses Xinerama, :1.0 and :1.1, if that makes any sense. The result looks something like this:

![img](../../images/scrot_xephyr_multihead.png "Xephyr in action")

Because I am currently working on the proper support, FrankenWM still uses the whole screen as one, rather than using a separate desktop for each monitor.
