Title: Listen to Pandora from your favourite shell
Date: 2013-09-06 15:06
Author: sulami
Category: Web
Tags: cli, onlineradio, pandora
Slug: listen-to-pandora-from-your-favourite-shell

After I was in holidays for two weeks, here some new tips for your life
with Linux. I love listening to music while coding. While I used to
listen to webradios, there are not really enough webradios for all my
musical needs. The big downside is, it needs to run in a graphical
browser, which is not always practical for me (e.g. on my netbook).

Now I found pianobar on reddit, a console-client for pandora. There are
a packages for openSUSE and Arch Linux, my main distros. The usage is
pretty simple: make a config-file at
*\$XDG\_CONFIG\_HOME/pianobar/config* or
*\$XDG\_CONFIG\_HOME/.config/pianobar/config*, add something like this

    user = abc@abc.com
    password_command = gpg --decrypt ~/passwd
    control_proxy = http://1.2.3.4:8080/

and you are ready to go. Password\_command reads your password from a
encrypted file, because you should never leave unencrypted passwords on
your computer. Control\_proxy is for people outside of the US (like me),
it only uses the proxy for control mechanisms, the music itself gets
streamed directly. You can get HTTP-proxies [here][].

  [here]: http://hidemyass.com/proxy-list/ "HideMyAss"
