---
title: Windows 10 - Post-Launch Thoughts
---

So it has been two weeks since the official Windows 10 launch, and there have
been a lot of discussions about it. I am going to summarize some of my own
thoughts, now that I had the chance to test it myself.

Windows 10 is the newest and [supposedly last version][last] of the ever so
popular operating system. [I have written before][concerns] about my concerns
regarding the continuing vendor lock-in, especially DirectX, so I will try to
not repeat myself.

From an end-user perspective, Windows 10 definitely feels better than Windows
8, which was crippled by design, due to the focus on the Metro view. Windows 10
brings back the "Start" menu, which I very much appreciate. The desktop in and
of itself feels okay-ish, but [lacks consistency in terms of UI style][const].
It certainly feels unfinished, not only visually, but also technically. On the
test machine used, an all-in-one Dell with some 2.5 GHz quad core and 12 GB of
memory, almost every window opened was white for roughly a second before it
loaded, which makes the whole system feel really unresponsive and unpolished.

But let us get to the real topic of discussion here, the obvious surveillance
"features". When Windows 10 launched, people saw the new installer for the
first time. The installer proudly promoted an "Express Installation" and almost
tried to hide the manual configuration, [which almost exclusively contained
privacy-related options][opts] that were all defaulting to invading your
personal space. I myself have written up [a small satirical piece][dickpics]
about it. Despite the majority of these options being hidden away, especially
after the installation, there are some options you cannot even deactivate at
all, like the [automatic transmission of "diagnostic and usage
data"][enterprise] (assuming you do not use Enterprise Edition).

Yesterday, a Czech analyst found out that [Windows 10 sends its keylogger logs
home regularly][czech], as well as scraping your drives after you search for
movie names (what might be the thought behind this, hmmm...), sending some 35
MB of encrypted data after using the webcam for the first time (hmmm, again),
and of course sending voice data, even when deactivating Cortana entirely. The
voice data alone makes up about 80 MB every 15 minutes.

There have also been [reports][ars], that these calls home are hardcoded in
such a way, that you cannot use the (internal) firewall to block them, and they
might leak your IP and other sensitive data if you are using a VPN or Tor.

In the end, while I have no particular desire to use Windows at all, with this
developments, I do not know if I even feel save around a Windows 10 machine
anymore, let alone using one. Sadly, the average user out there does not know
anything about this (I imagine), and they will be the ones violated. This is a
nightmare, and I desperately hope that Windows 10 will not catch on. Microsoft
has shown its true nature, its endgame, and it is bad. I do not think, anyone
can trust them anymore, at all.

[last]: http://www.theguardian.com/technology/2015/may/11/windows-10-last-version-microsoft
[concerns]: {filename}/thoughts-on-windows-10-and-free-software.md
[const]: https://imgur.com/a/ekRHU
[opts]: https://jonathan.porta.codes/2015/07/30/windows-10-seems-to-have-some-scary-privacy-defaults/
[dickpics]: https://gist.github.com/sulami/7e74e3de7db372d38402
[enterprise]: http://cdn.ghacks.net/wp-content/uploads/2015/07/telemetry.jpg
[czech]: https://translate.google.com/translate?hl=en&sl=cs&tl=en&u=http%3A%2F%2Faeronet.cz%2Fnews%2Fanalyza-windows-10-ve-svem-principu-jde-o-pouhy-terminal-na-sber-informaci-o-uzivateli-jeho-prstech-ocich-a-hlasu%2F
[ars]: http://arstechnica.co.uk/information-technology/2015/08/even-when-told-not-to-windows-10-just-cant-stop-talking-to-microsoft/

