Title: Django Tweaking: custom user model fixes
Date: 2014-04-19 22:20
Author: sulami
Category: Coding
Tags: django

I am currently finishing the initial coding of my newest, project, which will
be Peerwire (yes, another one), and because I like it, it is written in Python
using Django, specifically Django 1.6. Now I am using a custom user model to
add some additional fields, which is the only reason, I have to modify existing
apps I use and even Django itself. Here are some of the modifications I did so
far, in case someone encounters the same problems:

- ### Making Emails mandatory and unique

Edit *django/contrib/auth/models.py*, search for AbstractUser and change

    email = ...(blank=True)

to

    email = ...(unique=True)

- ### Using Cache Machine with your custom user model

Edit *django/contrib/auth/backends.py*, go to line 16 (I think) and change

    user = UserModel._default_manager.get_by_natural_key(username)

to

    user = UserModel._default_manager.get(username=username)

- ### Using django-registration with your custom user model

Edit *registration/forms.py*, in the top change

    from django.contrib.auth.models import User

to

    from django.contrib.auth import get_user_model
    User = get_user_model()

