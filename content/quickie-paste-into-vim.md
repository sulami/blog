Title: Quickie: paste into vim
Date: 2014-03-05 13:22
Author: sulami
Category: Coding
Tags: quickie, cli, vim
Slug: quickie-paste-into-vim

Pasting code into vim is painful, because vim tends to indent every line
further than the one before, which forces me to unindent everything
manually. But as I learned today, vim has a paste-mode which circumvents
this behaviour.

Just use the commandline to *:set paste*, paste your code and *:set
nopaste* again. Or, even better, bind this to hotkeys.
