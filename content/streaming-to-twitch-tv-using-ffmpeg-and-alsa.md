Title: Streaming to Twitch.tv using ffmpeg and ALSA
Date: 2013-05-14
Category: Web
Tags: alsa, ffmpeg, twitchtv

Well, as I lost my database, I could not recover this article, but I
still got my script, so here you go:

    INRES="1920x1080"
    OUTRES="1920x1080"
    FPS="30"
    QUAL="medium"
    STREAM_KEY="live_123_xxx"

    ffmpeg
    -f x11grab
    -s "$INRES"
    -r "$FPS"
    -i :0.0+0,0
    -itsoffset -00:00:01
    -f alsa
    -i hw:3,0
    -f alsa
    -i hw:0,0
    -filter_complex amix=inputs=2:duration=first:dropout_transition=3
    -af aresample
    -vcodec libx264
    -preset "$QUAL"
    -s "$OUTRES"
    -acodec libmp3lame
    -ar 44100
    -threads 6
    -b:a 512k
    -pix_fmt yuv420p
    -f flv "rtmp://live.justin.tv/app/$STREAM_KEY"

You get your stream key [here][1]. Adjust resolutions as needed (input
and output), aswell as FPS. In lines 13-16 you can change your audio
devices (speakers and microphone, get them from "*arecord -l" *for
microphones and*"aplay -l"*for speakers). Adjust threads to the amount
of CPU cores you want to use to encode.

  [1]: http://www.twitch.tv/broadcast "Twitch"

