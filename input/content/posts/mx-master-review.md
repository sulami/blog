title = "Logitech MX Master 3S Review"
timestamp = "2024-06-09"
tags = ["review"]
---

Recently my Magic Trackpad's battery started bulging enough to stop the feet in
the corners from reliably contacting the desk underneath, resulting in the whole
trackpad sliding around. In my ongoing quest to standardize as many connectors
as possible on USB-C, I bought a Logitech MX Master instead of replacing the
trackpad. What follows is a review after a couple of weeks using it. Overall I
think it is a competent mouse, but I will specifically highlight parts I do not
like, so this might come off as more negative than I actually feel.

# Hardware

I got the white Mac version, though it is still not entirely clear to me where
exactly it differs from the regular version. It has some software integrations
with macOS which might or might not be exclusive to this model, more on that
later.

The first thing I noticed was the surface finish, which is a kind of hard
rubber. I am still somewhat worried that it will become sticky after some time,
but at this point it is too early to tell. There is a button embedded on the tip
of the "wing" that forms the thumb rest, and that part definitely needs to be
somewhat flexible, but the remainder of the body could as well be regular hard
plastic, like all the buttons are. There are also some slight variances in gap
width where the top shell meets the silver rim of the bottom shell. One does
need to look out for this to notice it, but I would have expected better at this
price point. 

The main mouse buttons are relatively quiet and low-pitched in sound, a bit
louder than an Apple trackpad in silent mode, but enough to not be annoying.

I do enjoy the steel mouse wheels in their feel, much better than rubberized
ones. The horizontal one is dampened such it does not freewheel at all, but also
does not feel scratchy. In fact, it almost feels like it is magnetically held in
place, which it might very well be the case, as I cannot make out an axle. The
main scroll wheel has electromagnetic indents, which I will admit is pretty
cool. They can be turned on or off permanently, on-demand through a button
press, or even automatically when scrolling quickly, causing the wheel to spin
freely. There is an optional software feature called _Smooth Scroll_, which
emulates the trackpad acceleration curve, and I would definitely recommend.

The charging port is placed in the front, where anyone but Apple would place it,
and the mouse can charge just fine while in use. In a couple of weeks I have
charged the mouse once for an hour or two, and it has been hovering between 60
and 40% for the entire time, so no complaints about the battery life at all.

# Software

While I had an overall positive impression of the hardware, I am not sure yet
about the software. Technically the mouse functions without any software
support, but realistically one wants to install the Logitech app. Note that
there are actually two variants, _Logi Options_ and _Logi Options+_, and only
the latter actually supports this mouse.

The app is easy enough to use, and from what I understand does not strictly
require a Logitech account. It supports all the basic features, like selecting
the sensor sensitivity, adjusting scroll direction and speed, and rebinding all
but the two main buttons.

The second scroll wheel can be used for horizontal scrolling, which is what I
do, but it can also be used for switching applications or application tabs.[^1]
Both of those uses are hampered a bit by the lack of tactile indents, but they
are usable. A better alternative in my opinion is using it for zooming in and
out, which works just like pinch-to-zoom.

Both the thumb button embedded in the thumb rest and the top button behind the
main scroll wheel can be setup for simple actions or gestures. Gestures work by
holding down the respective button and moving the mouse. The zoom gesture again
mimics pinch-to-zoom. There are a few preset sets of gestures consisting of a
click action plus the four directions, as well as the option to define a custom
set. Annoyingly, the custom set does not include some of the nicer gestures like
zooming or panning, so if one wants to zoom via a vertical gesture, one has to
live with a useless rotate gesture in the horizontal direction.

One potentially useful feature is application-specific profiles. Basically one
can select any installed app, and if that app is active, the software changes
all settings to that app's profile, button mappings, sensitivity, etc. This
works because the software runs a daemon that intercepts all mouse input and
remaps it depending on the settings. This architecture enables a lot of
functionality, but also introduces an annoying flaw. If the daemon is not
running, or loaded up, the mouse falls back to some form of default setting.

One might think that is a niche problem, but it happens for a few seconds on
every system wake-up, or sometimes randomly when I presume the daemon is being
auto-updated and thus restarted. This means the mouse sensitivity and scroll
direction will be wrong, but also all button mappings are back to default,
including the button on the top that defaults to toggling the mouse wheel
indents. I have bound that one to switch applications, which is often one of the
first things I do when unlocking my computer. If I inadvertently unlock the
mouse wheel and do not re-lock it before the daemon gets activated, I need to
load up the Options+ app to re-engage the ratchet manually. This has led me to
develop a habit of wiggling the mouse for a few seconds until it changes
sensitivity, signalling that the daemon has loaded.

Another gripe I have is related to the fact that I switch between several
computers on a daily basis, namely my personal and my work computer. The mouse
can pair with up to three computers, and a button on the bottom switches between
them, which is much nicer than having to physically plug it into the new host,
but because all the settings are injected on the computer-side, they are all
per-computer. One feature that is not present in the app (yet?) is syncing
settings between computers, even with a Logitech account, so if I want my mouse
to behave the same on all my computers, I need to manually apply all settings
changes to all computers.

Overall I think it is quite interesting how they have moved a lot of
functionality to the host software, but I think it would be nice to at least
have the option to persist some settings in mouse storage. That way at least
some of those bumps could be smoothed over.

[^1]: Tab switching surprisingly worked in every single app I tried, not just
    browsers, but also code editors and terminals. I suspect that macOS has a
    generic tab selection API.
