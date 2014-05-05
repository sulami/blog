Title: Quickie: Markdown syntax highlighting in vim
Category: Coding
Tags: vim, quickie, markdown
Date: 2014-04-15 23:50
Slug: quickie-markdown-syntax-highlighting-in-vim
Summary: How to fix syntax highlighting for markdown files in vim

I save my Markdown files using the *.md* extension, like probably everyone
else in this world, which is fine. Not fine is vim interpreting *foobar.md* as
modula2-file, which disables proper syntax highlighting. The fix is quite easy,
but might not survive updates/patches.

Open vim's architecture-independent data directory, usually something along the
lines of */usr/share/vim/vim74/*. In there, there should be *filetype.vim*. If
you open it (with vim, of course...), search for "markdown", which will bring
you to the following line:

    " Markdown
    au BufNewFile,BufRead *.markdown,*.mdown,*.mkd,*.mkdn,README.md  setf markdown

This line tells vim that those files are probably Markdown files and should be
interpreted this way. Add \*.md to the list. Now search for Modula2, and remove
the \*.md entry from there, so nothing collides. When starting vim the next
time, it should identify Markdown correctly.

