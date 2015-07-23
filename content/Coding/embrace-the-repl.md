Title: Embrace the REPL
Date: 2015-07-23

As some of you may be aware, the tool of my choice is [vim][vim] (or
[neovim][nvim]). Two of the languages I write a lot in are Haskell and Python,
because both of these languages are very nice to quickly prototype something
in. Both of these languages also feature what is known as a [REPL][repl], a
*Read-Eval-Print-Loop*, a feature that has been around in Lisp since the
fifties. The name also comes from the Lisp origin:

    ::lisp
    (loop (print (eval (read))))

Todays, arguably more modern languages have more sophisticated REPLs, like
Haskell's [GHCi][ghci], Python's [IPython][ip], [BPython][bp], etc. each
coming with additional features to make testing something quickly really
simple.

It just happens to be that other text-editor, [emacs][emacs], that also uses
its own Lisp flavour as scripting language, also provides excellent support for
Lisp REPLs in the form of [SLIME][slime]. SLIME allows the user to evaluate
Lisp from a emacs buffer directly in a REPL in another buffer. I have seen this
functionalility at a talk recently and wanted to replicate it in vim. As it
turns out, there is something called [vim-slime][vslime]. Because vim cannot
directly handle Lisp, it uses a more general approach by using [screen][screen]
or [tmux][tmux]. It also handles things like `let` prefixes in Haskell, and
`:{`/`:}` for multi-line pasting. There is also special support for handling
the indentation levels in Python. But enough listing of features, a picture
says more than a thousand words, especially if it is a gif.

![vim-slime in action][gif]

[vim]: http://vim.org
[nvim]: https://github.com/neovim/neovim
[repl]: https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop
[ghci]: https://wiki.haskell.org/GHC/GHCi
[ip]: http://ipython.org/
[bp]: http://bpython-interpreter.org/
[slime]: https://common-lisp.net/project/slime/
[vslime]: https://github.com/jpalardy/vim-slime
[screen]: https://www.gnu.org/software/screen/
[tmux]: https://tmux.github.io/
[gif]: {filename}/raw/vimslime.gif

