Title: Raspberry Pi: an introduction to GPIO
Date: 2013-06-09 13:53
Author: sulami
Category: Hardware
Tags: gpio, python, raspberrypi
Slug: raspberry-pi-an-introduction-to-gpio

So you can do a lot of cool things with RasPis which are based on the
small size and power consumption of the device, but this small computer
has another advantage, a set of built-in GPIO pins. Those can be used
for simple control tasks using switches, leds, speakers and similar
stuff.

First thing you will need is a [diagram of the pin layout][]. On this
page there are also some easy examples of code in C, Ruby, Perl, Python,
Java, Bash, Pascal and Basic, so everyone should find a good language
for themselves, I personally prefer Python for almost every purpose.
There are some more explanatory examples for Python [over here][].

In terms of hardware, you can salvage switches, leds and piezo-speakers
from old computer cases, or buy them new from shops like Conrad or even
Amazon. On warning: the GPIO pins only run on 3.3V, 5V will fry your Pi,
but there is 5V permanent power if needed. Everything bigger than some
small control elements should be running on an external power supply.

  [diagram of the pin layout]: http://elinux.org/RPi_Low-level_peripherals#General_Purpose_Input.2FOutput_.28GPIO.29
    "elinux.org"
  [over here]: https://code.google.com/p/raspberry-gpio-python/wiki/Examples
    "Google Code"
