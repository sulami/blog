Title: OpenSSL-Bug: Update everything you got, like now!
Date: 2014-04-09 07:48
Author: sulami
Category: Web
Tags: homeserver, security, ssh, SSL, updates
Slug: openssl-bug-update-everything-you-got-like-now

Some days ago, a critical security bug named "[Heartbleed][]" was found
in the OpenSSL libraries, which basically means everything has to get
patched immediately, especially webservers, databases, SSH-Daemons,
anything that sort of relies on network communication. If you haven't
already patched your stuff, now is the time.

If you manage some sort of web- or mailserver, you should also get a new
certificate, as your old one might be stolen while the bug was not
discovered yet. All certificates used before OpenSSL was patched are
considered unsafe.

This is, what your bank website's certificate should definetely **not**
look like:

[![Screenshot from 2014-04-09 14:55:14][]][Screenshot from 2014-04-09
14:55:14]

(Yes, my bank's certificate is just over a year old. No, I will not
login before it is replaced)

  [Heartbleed]: http://heartbleed.com/ "Heartbleed.com"
  [Screenshot from 2014-04-09 14:55:14]: /images/Screenshot-from-2014-04-09-145514.png
