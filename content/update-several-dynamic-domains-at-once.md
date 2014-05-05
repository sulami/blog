Title: Update several dynamic domains at once
Date: 2013-07-24 14:05
Author: sulami
Category: Web
Tags: dnsomatic, dyndns, homeserver
Slug: update-several-dynamic-domains-at-once

As you might have noticed, I am hosting this blog and several other
websites on my own, and because I do not have a static IP (yet), I need
to use dynamic domains, which are updates every time I get a IP. Using
one of these domains is rather easy, but when it comes to updating
multiple domains at the same time, things can get a bit sketchy. What I
want to show you today is [DNS-O-Matic][], a service which accepts your
updates and forwards them to your DynDNS-providers. Now, if you want to
update it, this is the custom update-url to use:

*http://updates.dnsomatic.com/nic/update?hostname=all.dnsomatic.com&myip=\<ipaddr\>&wildcard=NOCHG&mx=NOCHG&backmx=NOCHG*

As a hostname to update, I use *localhost*, which simply updates
everything.

  [DNS-O-Matic]: https://www.dnsomatic.com/ "DNS-O-Matic"
