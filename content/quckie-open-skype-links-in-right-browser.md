Title: Quckie: open Skype links in right browser
Date: 2013-06-12 14:34
Author: sulami
Category: Web
Tags: quickie, chrome, firefox, skype, xdg
Slug: quckie-open-skype-links-in-right-browser

I currently use Google Chrome as my main browser on Linux because I need
the 11.7 Flash plugin. The downside was that I could not open links in
Skype conversations by clicking them, as they would just open in
Firefox, even though Chrome is my default browser. The solution on my
openSUSE box was quite simple:

    ~ > % xdg-settings get default-web-browser
    firefox
    ~ > % xdg-settings set default-web-browser google-chrome.desktop
