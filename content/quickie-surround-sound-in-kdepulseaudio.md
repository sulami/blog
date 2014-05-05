Title: Quickie: surround sound in KDE/PulseAudio
Date: 2013-06-05 15:47
Author: sulami
Category: Linux
Tags: quickie, kde, pulseaudio
Slug: quickie-surround-sound-in-kdepulseaudio

When using a surround sound setup like I do, you might want to use the
right settings for optimal results. On openSUSE, using KDE and
PulseAudio, there is a simple trick to make it actually work, otherwise
you won't get any sound output after switching to surround sound. First,
switch to surround sound on your desired output device in phonon. Then
you have to set the newly created output to default (on top) like this
in all lists:

[![sound][]][sound]

  [sound]: /images/sound.png
