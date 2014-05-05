Title: Quickie: fix vim in Cygwin
Date: 2013-08-12 13:51
Author: sulami
Category: Coding
Tags: quickie, cli, cygwin, vim
Slug: quickie-fix-vim-in-cygwin

So I was using Windows for some time, and the Windows-port of vim is not
as good, so I use Cygwin instead. By default, it looks and behaves like
vi, but even stranger. The fix is simple:

    cp /usr/share/vim/vim73/vimrc_example.vim ~/.vimrc

Be sure to merge possible own contents of your .vimrc.
