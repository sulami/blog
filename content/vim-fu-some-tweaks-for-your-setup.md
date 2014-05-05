Title: Vim-Fu: some tweaks for your setup
Date: 2014-01-04 18:37
Author: sulami
Category: Coding
Tags: cli, coding, typing, vim
Slug: vim-fu-some-tweaks-for-your-setup

Vim is awesome. Fact.

But if you like using vim as your main editor for everything, maybe even
mainly inside tmux inside ssh, like I do, there are lots of
optimizations available to rapidly increase your productivity.

#### General vimrc

Things you want to have in your vimrc:

    # line numbering
    set numbers
    # indentation python style, 4 spaces
    set smartindent
    set tabstop=4
    set shiftwidth=4
    set expandtab
    set smarttab
    # colored column, better than auto-linebreaks
    set colorcolumn=80
    # less errors
    silent
    # useful stuff, personal preferance
    set encoding=utf-8
    set fileformats=unix,dos
    # use jk to exit insert mode, use whichever key(combination) suits you
    imap jk 
    # new tabs on Ctrl-T
    map  :tabnew
    # file explorer on Ctrl-E
    map  :Explore
    # fix autocompletion, btw, it's built into vim 7+ and bound to Ctrl-N
    set completeopt=longest,menuone
    inoremap   pumvisible() ? "\" : "\u\"
    inoremap   pumvisible() ? '' : '=pumvisible() ? "\Down>" : ""'

#### Colors

Let's start off easy with the general vim look. If used in terminals,
those will handle parts of your coloring, but I will focus on coloring
vim itself. You probably want to use the same colorscheme for your
terminal and vim in any case.

To color vim use the *colo* command, preferably after you get some a
nice colorscheme from the interwebs. My favourites are [jellybeans][] or
[base16-tomorrow][] for dark and [Solarized light][] for light screens.
There are themes for shells and other stuff in this repo, have a look
around and read the docs.

You also might want to change background to dark using *set
background=dark*.

#### Pathogen

If you plan on using any plugins for vim, use [Pathogen][]. That's it.
It loads all your plugins automatically and makes installing and
removing plugins as fast as some seconds. The installation is pretty
straight-forward, just put pathogen.vim into your autoload directory,
add *execute pathogen\#infect()* to your vimrc and you are ready to go.
All other plugins go into *bundle/pluginname* from now on.

#### Powerline / Airline

One of my favourite plugins is Powerline, but I actually use
[Airline][], which is just written in vimscript and therefore portable
and does not require Python. Airline extends the status bar by a great
deal and gives you loads of useful information while looking cool.

Airline is sort of hard to install if you haven't done it before. Get it
and put it into the bundle directory, because you are using Pathogen,
aren't you? You then need the [right fonts][] and use them either in
Gvim (set guifont) or your terminal (Xresources, Xdefaults, whatever
kind of config you are using).

Some hints for your vimrc if you are using Airline:

    let g:airline_powerline_fonts = 1
    let g:aireline_theme = 'jellybeans' # just my favourite, there is a list on the GitHub pag
    set laststatus=2

This fixes some errors I encountered and forces an Airline theme.

#### Fugitive

Are you using git? If you aren't, you most likely want to. For your
work, your projects, your dotfiles, for everything. Versioning is nice.
Get [Fugitive][], it adds some git functionality directly into vim, and
plays nice with Airline.

#### Vinegar

You might have noticed how I bound Ctrl-E to :Explore. This is because I
use vim for selecting my files to edit most of the time. [Vinegar][]
improves the built-in filebrowser in a few ways and makes it a lot more
intuitive.

#### Nerdtree

Probably even better than Vinegar might be [Nerdtree][]. It adds an
additional vsplit with an interactive directory tree to quickly select
files to edit.

#### Surround

[Surround][] adds some new commands to vim concerning surrounding
characters like ",',(,[,{ and \<. You can do stuff like change
surroundings of where you are from single to double quotation marks by
hitting cs'" (first single, then double ones). Sort of useful and does
not hurt.

#### Commentary

[Commentary][] is a plugin that adds some actions regarding comments in
source code. Easily comment out blocks of code using *gc* saves you a
few seconds everytime, which might add up one day.

#### Repeat

When using the above plugins, vim does not recognize the added actions
to repeat them, namely the ones from surround and commentary. [Repeat][]
fixes this behaviour.

#### Trailing-Whitespace

[Traling-Whitespace][] is a simple one. It marks trailing whitespaces in
red and introduces the *:FixWhitespace* command to remove them.

  [jellybeans]: https://github.com/nanotech/jellybeans.vim "GitHub"
  [base16-tomorrow]: https://github.com/chriskempson/base16-vim/blob/master/colors/base16-tomorrow.vim
    "GitHub"
  [Solarized light]: https://github.com/chriskempson/base16-vim/blob/master/colors/base16-solarized.vim
    "GitHub"
  [Pathogen]: https://github.com/tpope/vim-pathogenhttp:// "GitHub"
  [Airline]: https://github.com/bling/vim-airline "GitHub"
  [right fonts]: https://github.com/Lokaltog/powerline-fontshttp://
    "GitHub"
  [Fugitive]: https://github.com/tpope/vim-fugitive "GitHub"
  [Vinegar]: https://github.com/tpope/vim-vinegar "GitHub"
  [Nerdtree]: https://github.com/scrooloose/nerdtree "GitHub"
  [Surround]: https://github.com/tpope/vim-surround "GitHub"
  [Commentary]: https://github.com/tpope/vim-commentary "GitHub"
  [Repeat]: https://github.com/tpope/vim-repeat "GitHub"
  [Traling-Whitespace]: https://github.com/bronson/vim-trailing-whitespace
    "GitHub"
