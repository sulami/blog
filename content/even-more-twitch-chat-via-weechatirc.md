Title: Even more Twitch: chat via WeeChat/IRC
Date: 2013-05-14 19:28
Author: sulami
Category: Web
Tags: irc, twitch.tv, weechat
Slug: even-more-twitch-chat-via-weechatirc

**NOTE**: This script is broken as the twitch.tv-server has changed.
Head over to my [updated version][].

After explaining how to stream to Twitch, here comes the cool way of
chatting there, using WeeChat, my personal favourite IRC-client. Again,
we will use a small shell-script to save some time.

    #!/bin/sh

    read -p "Enter Twitch.tv-Channel: " -e channel
    username='username'
    password="password"
    login="/connect irc://$username:$password@$channel.jtvirc.com/#$channel"
    weechat-curses -r "$login"

Change username and password to your Twitch-accout-data and be sure to
store it in a secure environment. Then chmod +x and run it. Enter your
desired channel and profit!

Some additional tips:

Your username/password should be letters/numbers only. I had some
serious headaches escaping special character so I just changed my
twitch-password to 30 random letters and numbers.

When using popular channels (like over a few hundred viewers), your chat
will be spammed with join-/leave-messages. To fix this just type

    /filter add joinquit * irc_join,irc_part,irc_quit *

inside weechat and it filters this spam. Only once required.

Users of other IRC-clients like irssi or XChat can copy the url-syntax
and feed it into the client.

  [updated version]: /twitch-irc-the-new-way.html
    "Twitch-IRC – the new way"
