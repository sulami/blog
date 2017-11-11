---
title: Hello, World! Again!
---

This is the new blog. It's now built using [Hakyll][hakyll] instead of
[Pelican][pelican][^switch], and it's using [Tufte CSS][tufte] instead of my
homegrown Jinja2 templates and CSS. I spent much more time on setting this up
than I care to admit. The upshot is, I have new CSS without having to rewrite
all my custom templates from scratch, and get to use these neat stylesheets.

[^switch]: Why switch, you ask? Shiny toys, that's why.

One of my major pains with my old setup was the setup required to just write
something quickly, due to the fact that I was maintaining my own styles and
sources in different repositories, and also because Python requirements are
notoriously janky if you're not careful. This new system is handled entirely by
[stack][stack], which makes things much easier.[^source] Initial setup takes
quite a while, because I have to compile a whole load of dependencies, but it's
a fully automatic setup.

[^source]: By the way, you can get the source of this blog [over here][source],
  if you want to steal some code, or just look at it.

I have to admit, I was tempted to just switch to Medium during this whole
process. But now that I have figured out how to build this whole
thing[^compile], I am quite happy with the results. Because Hakyll, just like
Pelican, uses Markdown as source to generate HTML, I could easily port over my
old posts once the dust had settled.

[^compile]: Read: get it to compile

[hakyll]: https://jaspervdj.be/hakyll/
[pelican]: https://blog.getpelican.com
[tufte]: https://edwardtufte.github.io/tufte-css/
[stack]: https://www.stackage.org
[source]: https://github.com/sulami/sulami.github.io
