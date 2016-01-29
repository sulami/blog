Title: Can Software Die?
Date: 2016-01-29
status: draft

Software can be classified as alive. Not using the [Wikipedia definition][wiki]
of alive, because that requires biological processes, but philosophically seen
software is kind of like a living organism. There are constant changes as the
software evolves, adapts to environmental changes and grows new features.

But what if it stops changing? Can software die? What about software that is
"done"? One of the most used tools on UNIX machines is `echo`. The current GNU
implementation which is arguably the most widely used one (assuming most OS X
users rarely use it) is at the time of writing [about 270 lines of code][gnu],
and is already [considered bloated by some][bloat].

But despite it being quite large and filled with features, especially compared
to its BSD counterparts, there have been [no meaningful changes in several
years][log]. Is `echo` dead, or is it just finished? As it stands right now,
there probably are [features that could be added][xprint], but none that make
sense. And with just 270 lines of code chances are that `echo` might actually
be entirely bug-free and always working as intended.

But do not go into paradox-induced freeze just yet, for I have a solution to
this dilemma. Software can be considered alive, but in a different way. In the
first paragraph I used the term "evolve". If we think of a piece of software as
a species that undergoes evolution and following that consider a single version
a member of that species, things start to make sense. A program can die, it can
crash or exit gracefully after having lived for its expected lifetime. A piece
of software can stop evolution, end even though [biological evolution
cannot][dawkins], it could effectively slow down to a halt if the perfect form
is reached, which is of course impossible in practice. And if no one runs a
piece of software anymore, it can go extinct.

So the answer is yes, software can die, but it does not happen when change
stops occurring. Because of the drastically more limited scope of software
compared to biological life, software has a real chance of achieving
perfection, a state in which no changes are required or desired.

  [wiki]: https://en.wikipedia.org/wiki/Life
  [gnu]: http://git.savannah.gnu.org/cgit/coreutils.git/tree/src/echo.c
  [bloat]: https://stackoverflow.com/questions/3290683/bloated-echo-command
  [log]: http://git.savannah.gnu.org/cgit/coreutils.git/log/src/echo.c
  [xprint]: https://bugs.freedesktop.org/show_bug.cgi?id=558
  [dawkins]: https://richarddawkins.net/2015/02/misconception-monday-can-evolution-stop/

