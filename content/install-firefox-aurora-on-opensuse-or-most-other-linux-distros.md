Title: Install Firefox Aurora on openSUSE (or most other Linux distros)
Date: 2013-07-11 23:50
Author: sulami
Category: Linux
Tags: aurora, firefox, flash
Slug: install-firefox-aurora-on-opensuse-or-most-other-linux-distros

When it comes to software, I like to have the new stuff. Like Firefox
Aurora, the pre-beta version of our favourite webbrowser. There are two
main problems with getting Aurora to run on openSUSE: first, you have to
get the right version somewhere, which is harder than it might sound,
because the official download page only ships the 32bit version, and
let's be honest here, who still runs 32bits? But do not even start
searching, Mozilla has binaries ready for us, they are just hiding them
[here][]. You probably want something like
*firefox-\<version\>.en-GB-x86\_64.tar.bz2*. Now that you got your new
browser, extract it somewhere (home-directories are a nice place) and
add a desktop icon (if there is some sort of desktop in your setup), an
icon can be found at *browser/icons*.

You will most likely find one major flaw: there are no plugins, like
flash or java. Those are located at */usr/lib/browser-plugins*, symlink
all of them to your new profile like this (edit your profile name, it is
random):

    ln -s /usr/lib64/browser-plugins/* ~/.mozilla/firefox/.default/plugins/

Symlinking will automatically update them when your package manager does
so, which is crucial for your security. When you are done, it looks
something like this (I use vimperator, which changes the UI a bit):

![aurora][]

  [here]: https://ftp.mozilla.org/pub/mozilla.org/firefox/nightly/latest-mozilla-aurora-l10n/
    "mozilla.org"
  [aurora]: /images/aurora-1024x752.png
