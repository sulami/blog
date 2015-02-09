Title: A practical solution for Twitch.tv
Date: 2015-02-08
Category: Web
Tags: twitchtv, alsa, ffmpeg, pulse

I have now written three scripts to stream to Twitch.tv from Linux, and all of
them were just hardcoded ffmpeg commands. Today I am proud to present an actual
streaming script that takes arguments, passes them to ffmpeg in the proper way
to enable streaming and parses the output. The code can be found here:

[https://github.com/sulami/streamo](https://github.com/sulami/streamo)

