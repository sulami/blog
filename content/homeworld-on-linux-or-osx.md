Title: Homeworld on Linux (or OSX)
Date: 2015-02-23
Category: Linux

Today I will write about something slightly different than usual. I will be
writing about a videogame, Homeworld. These days, Homeworld 1 and 2 are being
released in remastered editions, with support for current resolutions and so
on. This led me to playing the original. Homeworld 1 has been ported to Linux
by fans some years ago. All you need is an original game and access to a
Windows machine (or VM).

Install Homeworld on Windows, patch it to 1.05 and pack the complete game
directory to move it to your Linux machine. Then, mount the CD, get
`HW_Comp.vce` and `HW_Music.wxd` and put them in your game directory as well.
This enables you to play without the CD mounted. Then just get the prebuilt
binary for your OS (yes, OSX works as well) from [homeworldsdl.com][hwsdl] and
put it in the game directory as well (it is in `bin` in the .tar.gz). You might
also need to download some SDL libraries, but everything should be in your
distro's repositories. If you have done everything right, you can start
Homeworld from the shell using:

    :::shell
    SDL_AUDIODRIVER=alsa ./homeworld

If you experience sound issues under PulseAudio, make sure to install the
ALSA-plugin for PulseAudio. Using PulseAudio directly will result in glitchy
sound, so you have to proxy it through ALSA. Also reduce the ingame channels to
8 and the sound quality to low.

You can also start Homeworld on a separate X-Server (or in Xephyr) to preserve
your resolution (using the DISPLAY variable), but this might interfere with the
mouse in some cases. Instead, you can also use the parameter `/window` to open
it in windowed mode, which works pretty well.

