#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = u'sulami'
SITENAME = u'/dev/sulami >> blog'
SITEURL = 'https://sulami.github.io'

TIMEZONE = 'Europe/Berlin'
DEFAULT_DATE_FORMAT = ('%Y-%m-%d')
DEFAULT_LANG = u'en'

THEME = 'pelican-theme'

DEFAULT_PAGINATION = 5

# Uncomment following line if you want document-relative URLs when developing
#RELATIVE_URLS = True

MD_EXTENSIONS = (['codehilite'])

STATIC_PATHS = ['pictures', 'raw']
DELETE_OUTPUT_DIRECTORY = True
OUTPUT_RETENTION = (".git", ".gitignore", "favicon.ico")
DRAFT_SAVE_AS = ''

TAGS_SAVE_AS = ''
TAG_SAVE_AS = ''

RELATIVE_URLS = False
FEED_DOMAIN = SITEURL
FEED_RSS = None
FEED_ALL_RSS = 'feed/rss.xml'
CATEGORY_FEED_RSS = None
TAG_FEED_RSS = None
AUTHOR_FEED_RSS = None
TRANSLATION_FEED_RSS = None
FEED_ATOM = None
FEED_ALL_ATOM = None
CATEGORY_FEED_ATOM = None
TAG_FEED_ATOM = None
AUTHOR_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None

