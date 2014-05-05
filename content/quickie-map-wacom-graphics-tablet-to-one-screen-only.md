Title: Quickie: Map Wacom graphics-tablet to  one screen only
Date: 2013-07-16 13:01
Author: sulami
Category: Linux
Tags: quickie, gimp, wacom
Slug: quickie-map-wacom-graphics-tablet-to-one-screen-only

I just dug out my Wacom Bamboo Pen, a simple cheap graphics-tablet I
bought on sale some time ago and plugged it into my openSUSE workstation
to do some graphical work. Awesome as openSUSE is, it automatically
detects it and everything works out of the box, thanks to preinstalled
xf86-input-wacom. Now I use two screens and want the tablet mapped to
only one of them to keep the correct aspect ratio, which I could not
change in the mouse options, but there is a quick way of setting this
up. First, get your output devices (screens) using xrandr:

    % xrandr
    Screen 0: minimum 320 x 200, current 3840 x 1080, maximum 3840 x 1920
    DFP1 disconnected (normal left inverted right x axis y axis)
    DFP2 disconnected (normal left inverted right x axis y axis)
    DFP3 disconnected (normal left inverted right x axis y axis)
    DFP4 disconnected (normal left inverted right x axis y axis)
    DFP5 disconnected (normal left inverted right x axis y axis)
    DFP6 disconnected (normal left inverted right x axis y axis)
    DFP7 disconnected (normal left inverted right x axis y axis)
    DFP8 disconnected (normal left inverted right x axis y axis)
    DFP9 disconnected (normal left inverted right x axis y axis)
    DFP10 connected 1920x1080+0+0 (normal left inverted right x axis y axis) 598mm x 336mm
       1920x1080      60.0*+
       1680x1050      60.0  
       1400x1050      60.0  
       1600x900       60.0  
       1280x1024      75.0     60.0  
       1440x900       75.0     59.9  
       1280x960       60.0  
       1152x864       60.0     75.0  
       1280x768       60.0  
       1280x720       60.0  
       1024x768       75.0     70.1     60.0  
       800x600        72.2     75.0     60.3     56.2  
       640x480        75.0     72.8     67.0     59.9  
    DFP11 connected 1920x1080+1920+0 (normal left inverted right x axis y axis) 598mm x 336mm
       1920x1080      60.0*+
       1680x1050      60.0  
       1400x1050      60.0  
       1600x900       60.0  
       1280x1024      75.0     60.0  
       1440x900       75.0     59.9  
       1280x960       60.0  
       1152x864       60.0     75.0  
       1280x768       60.0  
       1280x720       60.0  
       1024x768       75.0     70.1     60.0  
       800x600        72.2     75.0     60.3     56.2  
       640x480        75.0     72.8     67.0     59.9  
    CRT1 disconnected (normal left inverted right x axis y axis)

As you can see, I use DFP10 and DFP11 as my outputs, DFP10 being my main
screen on the left. Now I use xsetwacom, which comes with
xf86-input-wacom, to tell the tablet to only use this output.

    % xsetwacom --list devices
    Wacom Bamboo Pen Finger touch           id: 12  type: TOUCH     
    Wacom Bamboo Pen Finger pad             id: 13  type: PAD       
    Wacom Bamboo Pen Pen stylus             id: 14  type: STYLUS    
    Wacom Bamboo Pen Pen eraser             id: 15  type: ERASER
    % xsetwacom --set "Wacom Bamboo Pen Pen stylus" MapToOutput DPF10
