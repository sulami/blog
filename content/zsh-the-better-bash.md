Title: Zsh: the better bash
Date: 2013-05-22 10:07
Author: sulami
Category: Linux
Tags: cli, zsh
Slug: zsh-the-better-bash

When it comes to shell, most people use bash. It is installed and
selected by default on nearly every \*nix system and does the job
reasonably well. But if you are using the shell a lot, like I do, you
can do better: get zsh, it is in every repository I know of and provides
some advantages.

First off, it is fully compatible to bash and it's scripts, so nothing
will suddenly break here. Zsh is faster than bash, which is always good,
although the only way for me to recognize is to run
ncmpcpp-visualizations in fullscreen, which results in horrible
lag/tearing in bash. But the most important advantage for me is the
stellar tab-completion. Instead of block giant parts of your screen with
possible completions, it only shows them as long as needed, and
overwrites the space afterwards with useful content. Also, tapping tab
multiple times will cycle through the multiple possible completions, as
you may know from vi(m). It can complete commands, parameters, options,
users, hostnames, you name it. As if this is not enough, you get stuff
like prompts on both sides and an optional vi-control-mode using the
command/insert-model.

Zsh can be enhanced even further by using plugins like
[syntax-highlighting as you type][]. Useful and pretty. I also recommend
having a look at [oh-my-zsh][], which is a framework for managing
settings, themes and plugins, if you plan to use a lot of them.

  [syntax-highlighting as you type]: https://github.com/zsh-users/zsh-syntax-highlighting
    "GitHub"
  [oh-my-zsh]: https://github.com/robbyrussell/oh-my-zsh "GitHub"
