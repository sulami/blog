Title: Quickie: disable auto-mute when skyping on PulseAudio
Date: 2013-09-27 10:12
Author: sulami
Category: Linux
Tags: quickie, pulseaudio, skype
Slug: quickie-disable-auto-mute-when-skyping-on-pulseaudio

When skyping on Linux (and elsewhere) I like to listen to music in the
background, but PulseAudio automatically mutes my Amarok and other
players as soon as Skype plays some kind of sound, which is insanely
irritating. The fix is quite simpe, open */etc/pulse/default.pa* in your
favourite edior (hopefully vim) and look for "cork" and "phone stream".
Comment this. Restart PulseAudio/your PC.

    ### Cork music/video streams when a phone stream is active
    #load-module module-role-cork
