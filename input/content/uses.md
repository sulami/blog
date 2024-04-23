title = "Uses This"
slug = "uses"
kind = "page"
---
I really enjoy reading [uses this](https://usesthis.com/) to get some ideas about what kind of tooling other people use, and of course to borrow ideas, so I thought I'd do my own version of this. This is also a page instead of a post, and I will try to keep it up to date, as my setup keeps changing.

 # Hardware

My workstation is optimised for portability, so everything is relatively small and packs well for travel, while maximising ergonomics:

- Apple MacBook Pro 16" (2021 model) - Competent computer, no complaints
- [Roost Laptop Stand](https://www.therooststand.com/) - Saves my neck and posture
- [Keyboardio Atreus Keyboard](/posts/atreus/) - Using the [Colemak layout](https://colemak.com); read my review [here](/posts/atreus/)
- Apple Magic Trackpad - Better for RSI, for me at least
- iPhone 15 Pro - Finally on USB-C
- Apple Airpods Pro (2nd Gen) - Not perfect, but pretty good

I try to only buy USB-C based devices at this point, which means I can use the same set of cables for everything.

# Workshop

Probably my most useful tool is my scope, a [Siglent SDS1104X-U](https://int.siglent.com/products-overview/sds1000xu/), which is punching well above its price class.

I used to have a Rigol DP832A power supply, but that was decidedly overkill and did not make it when moving countries, also due to it weighing over 10 kg. These days I use cheap single-channel switching supplies, which are adequate for digital and basic analogue work.

My preferred multimeter is the [Brymen BM235](https://brymen.eu/shop/bm235/). I sadly missed out on getting the blue version, so mine is red.

My soldering iron is the Miniware TS80P[^1] (also USB-C!) with [IronOS](https://github.com/Ralim/IronOS). I used to have a Weller WE1010, but similarly to the power supply it was not pulling its weight when it came to moving.

I also have a [Prusa Mini+](https://www.prusa3d.com/en/category/original-prusa-mini/), which is a great 3D printer for those less inclined to spend time tweaking their setup.

# Software

As I am on a MacBook, I use macOS, which to me is the only viable OS at this point.[^1]

If I write software for my personal use, chances are it is written in [Rust](https://www.rust-lang.org/).

My browser of choice is [Firefox Developer Edition](https://mozilla.org/en_US/firefox/developer/).

I am using a variety of text editors:

- Emacs is the most setup one
- Neovim for small editing tasks
- IntelliJ IDEA with IdeaVim for some large projects
- Visual Studio Code with the neovim plugin in some situations[^3]

All my configuration is [available online](https://github.com/sulami/dotfiles).

Because the default experience on macOS is not ideal, there is a list of additional software I use, that I would broadly describe as power user tools:

- [Alfred](https://alfred.app/)
- [Rectangle](https://rectangleapp.com/)
- [Contexts](https://contexts.co/)
- [Dash](https://kapeli.com/dash)

My notes are managed in [Obsidian](https://obsidian.md) at this point.

I have also gotten pretty proficient at [KiCad](https://kicad.org/) over the last few years, as well as [Onshape](https://onshape.com/), which I was sceptical about at first, but is a very capable CAD software for hobby use.

[^1]: It looks like Miniware have gone out of business or otherwise disappeared. I might opt for a [Pinecil](https://pine64.com/product/pinecil-smart-mini-portable-soldering-iron/) once I cannot source tips anymore.

[^2]: Windows is just plain unusable in my opinion, and while I have a soft spot for Linux/BSD, they just require too much upkeep.

[^3]: For example the Rust debugger is pretty good, though not as nice as IntelliJ's
