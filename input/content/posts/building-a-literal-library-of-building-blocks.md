title = "Building a Literal Library of Building Blocks"
timestamp = "2019-02-02"
---
This post[^1] is heavily inspired by [a remark Zach Tellman made on the defn podcast](https://soundcloud.com/defn-771544745/30-zach-tellman-aka-ztellman), where he says:

> Having been a professional programmer for a decade, I have a decade's worth of experience at writing stuff from scratch, not a decade's worth of tools in my toolbox. And that seems like a not optimal set of circumstances. *[Quote at 57:45]*

I have listened to this some time around Christmas, and this quote has kept me thinking over the past couple of months. What Zach is talking about is a project he is working on which would allow you to capture explorative programming in the branching fashion in which it happens. His example revolves around using a shell to perform some work, like extracting some specific values from a file.[^2] He explains how we work out these sequences of commands that to accomplish our goal, but never generalise them, but instead throw them away, just to write them from scratch the next time we encounter a similar problem. This rings true for all kinds of programming, not just shell scripting, though shell scripts are especially susceptible to this.

Like Zach, I believe this to be a suboptimal situation. Especially being a functional programmer, I believe in small, abstract building blocks, composition, and code reuse, rather than overly specific, bespoke solutions that have to be written from scratch every time. I am someone who tinkers a lot, and there is a lot of code I never commit anywhere. As a matter of fact, I have a habit of creating throw-away files or whole projects in `/tmp` just to play with something for anywhere between five minutes and a weekend. At the same time I also have a repository on my Github literally called playground, which contains all kinds of small things that I did not want to go through the hassle of creating a Github repository for.[^3]

This repository has allowed me to cannibalise some snippets of codes I used in the past, but it is not what I would call a comprehensive library of generalised solutions to problems I repeatedly face. And that has been hugely helpful already, for example I have written about path-finding using the A\* algorithm before, so I had a working implementation ready when I needed it for another project.

Having a library, in the worldly sense of the word, of useful, generalised snippets of code would institutionalise the knowledge of them. You would not have to remember how to invert a binary tree, because if you have ever played with binary trees you would already have an implementation handy, and it would be tried and tested, and performance-optimised.


# Practical Implementations

Having arrived at the decision of generalising and collecting useful snippets of code somewhere, we are now facing the question of where somewhere actually is, and how we distribute the snippets in a way that allows us to easily use them.

The simplest solution would be to maintain one or several collections of useful snippets, and just copy-pasting them into the code you are writing. While this is fast and simple, it does not facilitate innovation flowing in either direction. Updates to the generalised versions are not included in downstream products using them, and vice versa. The result would likely be a duplication of similar, but subtly different solutions to all kinds of problems, scattered over various projects. Bugs that have long been fixed in one of them might still be present in others.

The alternative solution is packaging your snippets, and using them as a library. Most of the practical implementation will depend on the programming language you are using, and what kind of projects you are usually working on. Zach Tellman himself has a Clojure library called [Potemkin](https://github.com/ztellman/potemkin), which is a collection of "some ideas which are almost good", and which he uses as a dependency for most of his other libraries.

While this incurs some overhead, namely the packaging of the library, it does come with a lot of advantages. Other people can benefit from your library. Depending on the scale of the overhead involved with building a library, splitting snippets by topic into "actual" libraries might make sense. It does require more abstraction, and more documentation, but that is not a bad thing. For a simple library with a handful of data structures or functions, writing a quick readme and some docstrings takes less than an hour.

There is still room for a default, catch-all library that is just for personal use and contains miscellaneous snippets without any particular topic, and it can be where new snippets end up first. If a section of it grows large enough, it can be extracted into its own library. The bottom line here is, if you write something that solves a problem, keep it somewhere, ideally where you can find it again. Even if it is not generalised or documented, it might come in handy in the future.


[^1]: I know, insert the obligatory "I haven't posted in a while" bit here.

[^2]: You should really go and listen to the episode, he has a lot of very great insights.

[^3]: Interesting aside: while creating a local repository has so little friction that I do it all the time, only a fraction of them ever touch Github's servers, as creating a repository through the web interface incurs so much friction.
