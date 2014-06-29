#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = u'sulami'
SITENAME = u'sulami\'s blog'
SITEURL = 'https://sulami.github.io'

TIMEZONE = 'Europe/Paris'

DEFAULT_LANG = u'en'

# Blogroll
# LINKS =  (('Pelican', 'http://getpelican.com/'),
          # ('Python.org', 'http://python.org/'),
          # ('Jinja2', 'http://jinja.pocoo.org/'),
          # ('You can modify those links in your config file', '#'),)

# Social widget
# SOCIAL = (('You can add links in your config file', '#'),
          # ('Another social link', '#'),)

THEME = '/home/sulami/build/pelican-theme'

DEFAULT_PAGINATION = 5

# Uncomment following line if you want document-relative URLs when developing
#RELATIVE_URLS = True

MD_EXTENSIONS = (['codehilite(noclasses=true,pygments_style=friendly)'])

STATIC_PATHS = ['images', 'download']

GOOGLE_ANALYTICS = 'UA-38330260-4'
RELATIVE_URLS = False
FEED_DOMAIN = SITEURL
FEED_ALL_ATOM = 'feed/atom.xml'
FEED_ALL_RSS = 'feed/rss.xml'
CATEGORY_FEED_ATOM = 'feed/%s.atom.xml'
CATEGORY_FEED_RSS = 'feed/%s.rss.xml'

