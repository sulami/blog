#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

# This file is only used if you use `make publish` or
# explicitly specify it as your config file.

import os
import sys
sys.path.append(os.curdir)
from pelicanconf import *

SITEURL = 'http://peerwire.de'
RELATIVE_URLS = False
FEED_DOMAIN = SITEURL
FEED_ALL_ATOM = 'feed/atom.xml'
FEED_ALL_RSS = 'feed/rss.xml'
CATEGORY_FEED_ATOM = 'feed/%s.atom.xml'
CATEGORY_FEED_RSS = 'feed/%s.rss.xml'

DELETE_OUTPUT_DIRECTORY = True

# Following items are often useful when publishing

#DISQUS_SITENAME = ""
#GOOGLE_ANALYTICS = ""
