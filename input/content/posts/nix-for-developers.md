title = "Lightning Introduction to Nix for Developers"
timestamp = "2020-11-27"
tags = ["nix", "best-of"]
---
# Motivation

This is the part where [Patrick Winston says](https://www.youtube.com/watch?v=Unzc731iCUY) I need to promise something you will gain by reading this. My promise is the following: This is a very quick introduction to setup Nix on a macOS (or Linux) machine, which will get you reproducible, sandboxed environments to do your development in, as well as atomic rollbacks.[^1]

There are many introductions to Nix, but this one aims for speed. I will be skipping over a lot of the fundamentals and only tell you what you absolutely necessary to get up and running. Nix is a large and complex system, but you can get some returns on your time investment within 15 minutes, and decide on delving in deeper later.

This guide is aimed at macOS, but most of it can be applied to Linux as well.


# Homebrew?

The de-facto standard package manager for macOS is [Homebrew](https://brew.sh/). While it is a passable solution for installing Mac apps, it has a few shortcomings, some of which can be especially problematic for developers.[^2]

-   **Sandboxing:** Homebrew packages are for the most part installed into `/usr/local/bin`, which means they are always available to everyone. This can lead to conflicts which can require manual modification of `$PATH` to resolve. Especially programming languages tend to hit this, as some software only runs on specific versions of their language.
-   **Freezing:** Even though Homebrew is based on git, is does not support explicitly installing specific versions of a package, or pinning the version in a lockfile.[^3] This by extension also means that whenever you install a homebrew on a new system, you will not be able to reproduce the exact versions installed on a known good system.
-   **Patching:** If you want to modify a package, you have to do it manually after installing, and potentially after every update. Homebrew can apply patches during the build process (via `brew edit`), but again, there is no declarative way of doing so.

Both the freezing problem and the patching problem can be circumvented by maintaining [your own tap](https://docs.brew.sh/Taps), but this comes with a significant maintenance burden, and I would not recommend it.

I would like to note that you likely cannot replace Homebrew entirely by Nix, as a lot of macOS-exclusive apps are not packaged in Nixpkgs. You could probably package them yourself if you really wanted to, but this has the same problems as maintaining your own Homebrew tap.


# Installing Nix

Before we can use it, of course we have to install Nix. I am using macOS, so I will also install nix-darwin. If you are using Linux, you can install [home-manager](https://github.com/nix-community/home-manager) instead for a declarative system setup.[^4]


## Nix

To get started, first we install the Nix package manager and language itself.

```sh
curl -L https://nixos.org/nix/install > /tmp/install-nix.sh
less /tmp/install-nix.sh  # inspect the script
sh /tmp/install-nix.sh --darwin-use-unencrypted-nix-store-volume
```

The extra argument is specific to newer Macs with a T2 chip. Refer to [the manual](https://nixos.org/manual/nix/stable/#ch-installing-binary) for more details.


## Nix-Darwin

Next we install [nix-darwin](https://github.com/LnL7/nix-darwin), which is essentially a framework written in the Nix language. It establishes a declarative configuration for the whole system, which packages are installed, all the way to [defaults](https://macos-defaults.com/). One of my personal selling points is management of [Launch Agents](https://developer.apple.com/library/archive/documentation/MacOSX/Conceptual/BPSystemStartup/Chapters/CreatingLaunchdJobs.html) in Nix, which is much nicer to manage than writing XML and working with `launchctl`.

```sh
nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
./result/bin/darwin-installer
```

The installer will prompt us with a few questions along the way, which do not seem to be well documented. Generally we want to respond with `y` throughout (the first one is optional).

```sh
Would you like edit the default configuration.nix before starting? [y/n]
Would you like to manage <darwin> with nix-channel? [y/n]
Would you like to load darwin configuration in /etc/bashrc? [y/n]
Would you like to create /run? [y/n]
```

At this point we are all set. We might need to start up a new shell to load the newly installed commands.[^5] If everything worked, we should now have `darwin-rebuild` in our `$PATH`.


# Declaring the System

The first use case we will be looking at is using Nix to setup our system as a whole.


## How Nix Works

I will interrupt here for a brief (and simplified) explanation of how Nix works in the first place. Essentially Nix works by building and installing software according to a set of recipes (Nix expressions) in what is called the Nix store, which is just a directory at `/nix`. To actually make the software available, it creates symbolic links to into the store in a profile, which is just another directory.[^6] This profile can then be added to `$PATH`, so that we can just use the software installed. The beauty of the symbolic links is that we can create many profiles which link to different sets and/or different versions of software in the store.

This also allows us to version profiles, and switch atomically between them, because every time we run `darwin-rebuild switch`, a new profile is created and activated. Should anything break, we can just switch back to the old profile. In practice this means running `darwin-rebuild --rollback`. We can also switch to a specific version, using `--list-generations` and `--switch-generation` if we want to rollback more than one change.


## Installing a Package

Before we can install a package, we need to find it first. Finding a package is as simple as running

```sh
nix search some-package
```

Let us modify `$HOME/.nixpkgs/darwin-configuration.nix` now. If we open that file, we should find a section similar to this:

```nix
environment.systemPackages =
  [ pkgs.vim
  ];
```

This is where nix-darwin declares the packages installed on the system. Go ahead and add a package to that list. Nix does not use commas to separate list items, just whitespace. The canonical package to add is `pkgs.ripgrep`, but any will do. Rebuild the system:

```sh
darwin-rebuild switch
```

We should now have `rg` in our `$PATH`, without having to open a new shell, as `$PATH` did not actually change. The [nix-darwin manual](https://daiderd.com/nix-darwin/manual/index.html) has a big list of configuration options that might also be interesting, but are not required now.


## Fetching Updates

As mentioned above, anything we build and install is controlled by our local Nix expressions in the Nix store. These are just build recipes in the Nix language, similar to Makefiles. The expressions usually pin a specific version of the software they build, and they themselves are also versioned. This means to update our packages, we need to update the expressions, which we do like so:

```sh
nix-channel --update
```

This fetches the latest versions of all channels we follow and updates our local Nix expressions accordingly. If a software definition got updated upstream, we can now rebuild it to get the updated version. Because channels are also versioned, we can even rollback channel updates if an upstream update broke for us.

To actually rebuild the packages according to the new definitions, we have to build a new version of our profile:

```sh
darwin-rebuild switch
```


# Using nix-shell

There is another way of using Nix than installing all packages system-wide. If we just want to try out a package without having to rebuild our system (and reverting afterwards), we can simply run

```sh
nix-shell -p some-package
```

Nix will build the package in the Nix store and drop us into a shell that has access to the package. Add `--pure`, and we get a completely clean environment except for anything that we explicitly add to the shell. This can be useful if the mere existence of a system-wide piece of software is problematic.

If we use this method to setup a Nix environment for a specific project, we can use a `shell.nix` file to declaratively[^7] express the environment like so:

```nix
{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    python-2.6
  ];
  PYTHONDONTWRITEBYTECODE = "1";
}
```

This looks complicated, but this does two simple things: anything in `buildInputs` is made available to the shell, and anything else is injected as an environment variable.[^8] Just calling `nix-shell` in the same directory will automatically pick up this file and execute it.

In this case we are simply getting an older version of Python, and also setting a related environment variable. Anyone using this configuration will have the same environment, which mirrors some of the benefits of Docker, but without the overhead of running containers.[^9]


# How to Debug Problems

This is the hard bit about Nix, the documentation is almost infamously sparse, and common recommendations are to either find an existing solution for your problem, or to read the Nix code involved. Because Nix includes a whole programming language, it allows users to build their own abstractions, which means that many packages have their own way of doing things.<sup><a id="fnr.10" class="footref" href="#fn.10" role="doc-backlink">10</a></sup>

I wish I could provide a sure way to solving all your Nix-related problems, but a lot of it comes back to pasting error messages into search engines and asking people online. Nix is not without its rough edges, and sooner or later you will run into one of them. I consider them learning opportunities, but they can be very frustrating.


# Where to Go From Here

This is just the beginning, there are many more parts of Nix to discover. It is probably advisable to read through the [Nix Pills](https://nixos.org/guides/nix-pills/index.html) to get a better understanding of the language and system.

[home-manager](https://github.com/nix-community/home-manager) is a project which manages a per-user Nix environment in a declarative way. If you are using nix-darwin it is somewhat optional, but can still be useful to build a more portable configuration.<sup><a id="fnr.11" class="footref" href="#fn.11" role="doc-backlink">11</a></sup> It can be installed as a nix-darwin plugin as well.

If you are looking for better project environment management with Nix, there are a few very useful tools. [Niv](https://github.com/nmattia/niv) allows you to declare and pin dependencies for a project. [Lorri](https://github.com/target/lorri) is a daemon that automates a lot of the `nix-shell` setup we have been doing by hand above, such as automatically loading and reloading an environment when you enter a project directory. [Direnv](https://direnv.net/) and [shadowenv](https://github.com/Shopify/shadowenv) are alternatives to lorri.

You might also want to try packaging some of your own software in Nix, or software that is not in [Nixpkgs](https://github.com/NixOS/nixpkgs) (yet). It is good exercise to gain a deeper understanding of the system, and as a bonus you get a more reproducible setup. Nix is a great fit to distribute internal developer tooling as well. I might write something on how to do this in the future.

Last but not least, if you really enjoy using Nix, you might want to try running NixOS, a whole Linux distribution which is configured using Nix.


[^1]: This is really just what Nix promises.

[^2]: A lot of the same applies to the various Linux distribution package managers.

[^3]: [*Because Homebrew doesn't work that way.*](https://github.com/Homebrew/homebrew-bundle/pull/552)

[^4]: You can also use home-manager on macOS, but the main benefit gained is dotfile management in Nix, which I do not consider necessary in the context of this article. More on this later.

[^5]: If we do not have the new commands, we might be missing the shell hook. Check out `/etc/static` for `bashrc` or `zshrc` and make sure we load the appropriate one in our shell configuration.

[^6]: It is actually symbolic links all the way down but that is not really important for now. [Symbolic links graphic](https://nixos.org/manual/nix/stable/figures/user-environments.png)

[^7]: For extra declarativity, we can also pin Nixpkgs to a specific revision.

[^8]: There are actually a few more valid options, which we can get directly [from the source](https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/mkshell/default.nix).

[^9]: Which is especially slow on macOS due to the virtual machine required.

<sup><a id="fn.10" class="footnum" href="#fnr.10">10</a></sup> It is [The LISP Curse](http://winestockwebdesign.com/Essays/Lisp_Curse.html) all over again. There is a fair degree of standardisation in Nix though.

<sup><a id="fn.11" class="footnum" href="#fnr.11">11</a></sup> Mainly if you want to reuse or port your configuration to a non-Darwin system. Using nix-darwin only for Darwin-specific tasks makes this much easier.
