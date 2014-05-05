Title: Quickie: Gnome 3 dark theme
Date: 2014-01-23 13:22
Author: sulami
Category: Linux
Tags: quickie, firefox, gnome
Slug: quickie-gnome-3-dark-theme

Many people, me included, prefer their computer screen mostly dark. And
while I quite like the default gtk-theme of Gnome 3, called "Adwaita",
it is white. Using *gnome-tweak-tool*, you can easily color it dark, but
this only works for Gtk-3 apps, so Firefox for example is still white.
To fix this, get *gtk-murrine-engine* (package name on Fedora) and [this
theme][]. Unpack it to */usr/share/themes*. Then use the tweak-tool and
*gtk-chtheme* (also a Fedora package name), to select "Adwaita-Dark".
Then login again or reboot for good measure.

  [this theme]: http://goo.gl/6dRiC2 "Dropbox"
