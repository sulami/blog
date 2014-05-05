Title: Quickie: use Python 3 features in Python 2
Date: 2013-06-11 11:50
Author: sulami
Category: Coding
Tags: quickie, python
Slug: quickie-use-python-3-features-in-python-2

I am currently writing on a Python-based program for servers, and being
servers they usually only bring Python 2.7 or 2.6. But I like the
improved Python 3 so much, that I import parts of it to Python 2:

    #!/usr/bin/env python

    from __future__ import print_function

    print("It works!")
