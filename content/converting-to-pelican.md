Title: Converting to Pelican
Date: 2014-04-15 00:40
Category: Web
Tags: news, pelican, python
Slug: converting-to-pelican
Summary: I converted Peerwire.de to Pelican

I converted Peerwire.de to [Pelican][], a Python-powered blog generator, mainly
because I want to get rid of PHP where I can. Previously, this blog was running
on selfhosted Wordpress, which is quite powerful, but for my blogging needs
too complicated and oversized.

Pelican is a static site generator written in Python, which means I maintain a
directory on my server with a bunch of markdown files containing my posts and
whenever needed, Pelican regenerates the whole page to plain static HTML.
Given that I write maybe one post every week and regenerating takes about 3
seconds, I save a huge amount of processing power by providing static content
as opposed to dynamic content. And besides that, Wordpress is a mess to
administer, now I just pop up vim and start writing, we scrapped the search,
the spam-filled comments and several MBs of overcomplicated PHP.

In my eyes, PHP is sort of dead anyway. Which does not hinder anyone from using
or even continue developing it, but the concept is just from yesterday. Today,
we can use full-sized programming languages like Python (Django) and Ruby
(Rails) to easily develop and maintain complex web-applications with
centralized, managed configurations. Most of the work gets done by those
frameworks, so you can concentrate on actually developing something new.

  [Pelican]: http://getpelican.com "Pelican"

