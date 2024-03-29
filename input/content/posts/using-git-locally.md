title = "Using Git Locally"
timestamp = "2014-07-15"
tags = ["git"]
---
When you write your code, using a version control mechanism like git is crucial, even more than writing proper unit tests, even when working on your own, even when it is only your evening project. Just use git and commit regularly, it will not hurt you (unless your workflow is inherently broken, but then fix you have to fix it anyway). Committing after every logical change and pushing when finishing the session is something I practice almost like a religion. Not only does it keep my history accessible and enables me easy branching and stashing (read up on [git-branch(1)](http://linux.die.net/man/1/git-branch) and [git-stash(1)](http://linux.die.net/man/1/git-stash) if you do not already use them), it also serves as simple way to mirror my repositories to other machines and backup them this way.

Now to have your code available everywhere, you can just push it to your [Github](https://github.com) or [BitBucket](https://bitbucket.org) account, but there are pieces of code we do not want to see there. Security-critical code, configs or other files containing sensitive data, or just code we are not particularly proud of. To still benefit from the network capabilities of git, we need to host git ourselves.

Setting up a local git mirror is not a big deal at all as the following examples will show. Git uses ssh to transmit data over the network, which means every toaster that is capable of running sshd should also be able to serve git repos in a way that Github or Bitbucket do it, just without the web interface. We assume we already have a repo containing out working directory, maybe even some commits, does not matter. The following snippet demonstrates establishing a git repo in my home directory which we can use to clone, push and pull.

```sh
% mkdir -p ~/git/myrepo.git
% cd ~/git/myrepo.git
% git init --bare
Initialized empty Git repository in /home/sulami/git/myrepo.git
% cd ~/myrepo
% git remote add origin /home/sulami/git/myrepo.git
```

That is it, my local repository is now registered as `origin` in this specific working copy, and once I pushed my commits to it, I can clone it to somewhere else. This is usually not all that useful, I only keep a bunch of bare repositories for archiving reasons around, to clone some old projects of mine if I want to look something up. Local remote repositories are way more interesting because we can use them to push changes around to various machines, work together with others more efficiently and much more. So here is how we setup a local remote git server, given our repo alreay exists like before. My ssh login uses my ssh-key to sign me in.

```sh
% ssh sulami@remote
% mkdir -p /srv/git/myrepo.git
% cd /srv/git/myrepo.git
% git init --bare --shared=group
Initialized empty shared Git repository in /srv/git/myrepo.git
% exit
% cd ~/myrepo
% git remote add origin ssh://remote/srv/git/myrepo.git
```

Do you recognize this snippet? It is almost the same as before, we just changed some details. First, we initialize the bare repository via ssh on our remote. We also set it up in a way that allows us to share it with a group we can add later on, so eventual coworkers can use their own ssh-logins to use this repository. You can now think of exposing it to the web, or maybe installing [cgit](http://git.zx2c4.com/cgit/) to provide a browser-friendly interface. It all depends on what you need, but to just synchronize your code amongst several machines there is nothing else nessecary.
