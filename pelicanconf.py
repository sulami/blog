#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = u'sulami'
SITENAME = u'/dev/urandom > blog'
SITEURL = 'https://sulami.github.io'

TIMEZONE = 'Europe/Berlin'
DEFAULT_DATE = (2012, 3, 2, 14, 1, 1)
DEFAULT_LANG = u'en'

THEME = '/home/sulami/build/pelican-theme'

DEFAULT_PAGINATION = 5

# Uncomment following line if you want document-relative URLs when developing
#RELATIVE_URLS = True

MD_EXTENSIONS = (['codehilite(noclasses=true,pygments_style=friendly)'])

STATIC_PATHS = ['images', 'download']

RELATIVE_URLS = False
FEED_DOMAIN = SITEURL
FEED_ALL_RSS = 'feed/rss.xml'
CATEGORY_FEED_RSS = 'feed/%s.rss.xml'

