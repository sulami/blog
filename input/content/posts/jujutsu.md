title = "jj is the better Git CLI"
slug = "jujutsu"
created_at = "2025-08-30"
tags = ["git"]

---

I first tried [Jujutsu] earlier this year, but could not get it to stick. One of
the main pain points was the lack of accurate documentation. Since then, the
documentation has got a lot better, and I have adopted Jujutsu for most of my
daily work, and I think it is the best Git interface currently available.

This might come as a surprise, as Jujutsu is generally a distinct VCS from Git.
The key here is that Jujutsu is built with the ability to swap out the [storage
backend], and the only production-ready backend at this time is the Git one.
This works well against the network effect, as one can use Jujutsu to work on
Git repositories stored on GitHub without anyone else even knowing. No need to
have your whole team buy into it.

Out of all the different Git interfaces out there I have used so far,[^1] the
best one has been [Magit]. Where Magit excels for me is the staging interface
and the ability to rewrite commits more easily. The former is not needed if
there is no staging area. For the latter, Magit has a number of handy shortcuts
based on interactive rebases, which is slightly quicker than the manual
equivalents, but still quite brittle. [I am a stickler for clean
history][clean-history], but even I find it difficult to convince folks to jump
through the necessary hoops.

Virtually all alternative Git interfaces focus on nicer presentation, graphs and
diffs mainly. What sets Jujutsu apart is that it instead envisions an entirely
different workflow, which enables it to deliver a significantly better user
experience. Even though it has not yet reached version 1.0, it already feels
more cohesive and user-friendly than Git, which is known for its idiosyncrasies
that result from 20 years of extension.

## Commits and Bookmarks

Jujutsu does not have branches, but it has bookmarks for Git interoperability.
New changes usually start out in empty commits, without a description, and
without a branch. They can be bound to a manually or automatically named
bookmark to push them to a branch on e.g. GitHub when they are ready. This
reduces the need to come up with branch names when they are not important. Once
a branch is deleted on the remote, like after merging, the next sync operation
will remove the corresponding local bookmark as well, removing the need for
manual cleanup.

`jj` commands also highlight the required hash prefix length to uniquely
identify a commit, so any commit that is referred to directly is usually at most
three characters long, which makes a big difference when dealing with
pseudo-random strings.

Beyond that, Jujutsu has [a somewhat elaborate DSL][revsets] to describe
revsets, sets of commits. This language can be used to define what is shown for
e.g. `jj log` or `jj diff`, but also for writing commands such as `jj squash`.

## Rewriting Commits

Jujutsu's entire workflow is built around rewriting commits. As there is no
separate staging area, to build up a change one continuously rewrites a commit
until it is done. This means modifying commits later on is no different from
working on a new commit.

There are a few different workflows, the [squash] workflow uses `jj new
<target>` to create a new empty commit off of a target commit to then eventually
`jj squash` the changes in. This is somewhat comparable to a Git `fixup`, but
more streamlined. Users of the [edit] workflow skip the intermediate commit and
instead directly edit the target commit by checking it out and making their
changes. Jujutsu automatically applies the changes as they happen and rebases
downstream commits. A third option is using the [`jj absorb`][absorb] workflow,
which takes the current commit's changes and distributes them into the ancestor
commits that last changed the relevant lines. It is basically a smart `fixup`
that can fix up several commits in one go. In my experience it is hit-and-miss,
but one can always undo, as we will see below.

Splitting commits in Git is [somewhat involved][git-split], but Jujutsu has `jj
split` which does exactly that. Select some changes from the current commit to
go into the first commit, the rest goes into the second commit, the descendants
get rebased on top. Similarly, removing a commit is as simple as `jj abandon
<target>`, the rest is automatic.

To move commits around in the history, they can also be rebased easily, and in
several different ways. Whole branches of the history tree can be rebased,
selected commits and their descendants, and even single commits without their
descendants. Rebased commits can be inserted as a separate child branch of a
target commit, or inserted between a target parent and its children.

## Conflicts

When talking to folks about rewriting commit history, one major pain point are
conflicts. Conflicts in Git break the entire repository until resolved, and they
tend to snowball downstream, growing in size with every commit affected. Jujutsu
on the other hand has [a novel approach to conflicts][conflicts], allowing
commits to be conflicted. This allows for easier handling and resolution of
conflicts, such as resolving them in separate commits, and it also reduces the
number of conflicts encountered when rebasing. Notably, resolving conflicts is
not a dramatically different workflow from regular development.

## The Operation Log

If you have ever badly broken a Git repository, you will be familiar with the
`git reflog` command, which contains all changes to references such as branches.
In some situations that allows undoing grave mistakes by referring to a commit
that might be gone otherwise. Jujutsu takes this concept a step further with the
[operation log], which records all changes to the repository. This enables one
of the most powerful commands, `jj undo`, which can undo any change. This ranges
from the trivial operations such as checking out a different commit, all the way
to undoing commits, merges, squashes, and rebases.

## Workspaces

A somewhat niche but nevertheless useful feature are workspaces. Basically, they
provide the ability to have several checkouts of the same repository in
different directories, but without having to sync them manually in any way. This
can be useful for example when running a long compilation or test suite on one
commit, and working on a different commit at the same time.

[^1]: [Magit], [lazygit], [gitui], [gitu], [the JetBrains one][jetbrains],
    [edamagit], and of course the CLI with a lot of aliases

[Jujutsu]: https://jj-vcs.github.io/jj/latest/
[Magit]: https://magit.vc/
[lazygit]: https://github.com/jesseduffield/lazygit
[gitui]: https://github.com/gitui-org/gitui
[gitu]: https://github.com/altsem/gitu
[jetbrains]: https://www.jetbrains.com/help/idea/using-git-integration.html
[edamagit]: https://github.com/kahole/edamagit
[storage backend]: https://jj-vcs.github.io/jj/latest/glossary/#backend
[clean-history]: /posts/cleaning-up-git-history/
[squash]: https://steveklabnik.github.io/jujutsu-tutorial/real-world-workflows/the-squash-workflow.html
[edit]: https://steveklabnik.github.io/jujutsu-tutorial/real-world-workflows/the-edit-workflow.html
[absorb]: https://www.pauladamsmith.com/blog/2025/08/jj-absorb.html
[git-split]: /posts/cleaning-up-git-history/#splitting-a-commit
[conflicts]: https://jj-vcs.github.io/jj/latest/conflicts/
[operation log]: https://jj-vcs.github.io/jj/latest/operation-log/
[revsets]: https://jj-vcs.github.io/jj/latest/revsets/

