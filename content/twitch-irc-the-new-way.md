Title: Twitch-IRC - the new way
Date: 2013-10-07 14:40
Author: sulami
Category: Web
Tags: irc, twitch.tv
Slug: twitch-irc-the-new-way

Some time ago, my twitch-irc-script stopped working, because, as it
turns out, twitch.tv changed their IRC-settings. The new way is even
easier, as they no longer use one server per channel but have all
channels on one server.

Server: irc.twitch.tv 6667

Nick: \<your twitch.tv-username\>

Password: Go to <http://www.twitchapps.com/tmi/> and get an oauth-token.
Do this only once, as it will change every time you visit the site. This
token, including the "oauth:"-part is your server password.

That's it, save it into your irc-client, connect and join your channel.
