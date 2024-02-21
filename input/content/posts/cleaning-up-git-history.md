title = "Cleaning Up Git History"
timestamp = "2021-07-26"
tags = ["git", "best-of"]
---
A clean git commit history is often underrated and can be immensely useful to ease code reviews and understand changes in the future (potentially in the midst of an outage).

Of course we are talking about the final history here as committed to the shared repository, not the intermediate history while we are working on the code. Sometimes the intermediate history is good enough to be pushed directly, but this is actually fairly rare.

Cleaning up the history might seem tedious at first for marginal cosmetic benefits, but it gets much easier and faster with practice. Here I am collecting some tips for cleaning up a git commit history before publishing it to others, for example in the form of a pull request.[^1]


# What Makes A Good Commit?

A clean commit history is one that is comprised only of good commits. But what makes a commit good? There are two parts to this, the content of the commit, and the commit description.

There are many standards for commit messages, but I personally use the [OTP commit guidelines](https://github.com/erlang/otp/wiki/writing-good-commit-messages) as the base. I then try to include any important technical and product consideration in the message, such as the motivation for the change, and maybe technical nuances such as alternative implementations considered and tradeoffs. See [Dan Carley's amazing example](https://github.com/alphagov/govuk-puppet/commit/63b36f93bf75a848e2125008aa1e880c5861cf46).

The content of the commit is a bit more fuzzy, but here is what I am looking for:

Every commit should make sense by itself. There is a *just right* size for commits, where there is nothing to add and nothing to remove. We want to add/remove/change exactly one thing in each commit. If we are struggling to come up with a good commit summary, that is a hint that our commit is not well scoped. Ideally this means a reviewer can review our changes commit-by-commit. We try to tell a story in commits.

Every commit should be runnable, that is we should be able to `git checkout` any commit and get a functional code base. This means no "*WIP*" commits or commit chains that only restore functionality at the end. This is important so that we can revert or rollback to any commit if things go sideways.


# Techniques

Git is very flexible, and for many of the techniques described here there are faster ways of performing the same actions. I am describing multi-step methods which preserve several individual commits for longer, as it is much easier to combine two commits than to split a single commit.[^2]

We are also using the long version of all flags for educational purposes. Refer to the man pages for the short versions.

We will be making heavy use of interactive rebasing, which can be triggered with

```sh
git rebase --interactive <ref>
```

Interactive rebase is incredibly powerful and can be used to perform many different operations. We can use it to avoid having to memorise many other commands that are specifically tailored to doing just one thing.

It is also usually used differently from regular rebasing, where we move one or several commits onto a different base (hence rebasing). When rebasing interactively we usually want to edit all commits between `HEAD` and `<ref>` instead. Thus we need to chose `<ref>` as the other end of the range we want to edit.

When you start an interactive rebase, a special file will be opened in `$EDITOR` with all commits in the rebase range and actions for them. Git will insert a guide below the list describing the available actions. Once we are happy with the actions, we confirm the plan and the actions resolve.[^3]


## Rewriting Commit Messages

While I encourage the writing of the full commit message directly at commit time, sometimes we can think of additions or changes to earlier commit messages. To change just the commit message of an existing commit, we can use:

```sh
git commit --fixup:reword=<ref>
```

These commands create additional "fixup commits" which are appended to the end of our history. Each of them is linked to another commit via its title. To actually combine the two (or more), we use:

```sh
git rebase --interactive --autosquash <ref>
```

This should setup the interactive rebase to do the right thing, so we just confirm the actions proposed by saving and closing the file.


## Adding to Existing Commits

Let us assume we have written a commit, and then notice a small change necessary that falls within the scope of this commit. Instead of writing a new commit, we want to just fix up the existing commit. This is done simply by running:

```sh
git commit --fixup=HEAD
```

If we notice our error only after adding additional commits, and want to retroactively add a change to an earlier commit, this command becomes:

```sh
git commit --fixup=<ref>
# Could be a SHA, or also e.g. HEAD~2
```

Just as before, this creates fixup commits that can be combined with an interactive rebase and the `--autosquash` flag.


## Combining Commits

If we have two already existing commits in our history that we would like to combine into a single one, we can use an interactive rebase, move the second commit after the first one if required, and select the `squash` option.

When resolving the rebase, git will stop at this point and open up a combined commit message for us. We definitely want to edit this one instead of sticking with the default concatenation of the two original messages.


## Reordering Commits

If we want to change the order of commits, you have guessed it, we can do this with an interactive rebase and just changing the order of the commits in the list.


## Splitting A Commit

If a commit turns out to contain several independent changes, we can opt to split the commit to isolate those changes. Again we use an interactive rebase, but this time we select the `edit` action for the commit we want to split. This will cause the rebase to pause when we reach this commit. At this point we want to use

```sh
git reset HEAD
```

and then start creating the new commits. We can use

```sh
git add --patch
```

to incrementally add sections of our files. This works best when editing existing files, as the patch interface is a bit lacking, but it works.[^4] We can then use

```sh
git commit
```

to create the new commits. Once we are done creating new commits replacing the old one, we can use

```sh
git rebase --continue
```

to finish the rebase.


## Removing A Commit

If we find ourselves with a commit that we decide we just do not need anymore, we can simply remove it from the history. This could be a change that we made but later realised we did not need after all, or something like the GitHub "update branch" merge commit noise.

The easiest way to do this is just using an interactive rebase and selecting the `drop` action for the commit in question.


# Further reading

The git man pages are split by command, so for documentation on `git rebase`, check `man git-rebase`. The man pages are good, but more of a reference than a usage guide.

[Oh Shit, Git!?!](https://ohshitgit.com/) has a memorable domain and provides some information for how to perform self-rescue after a lot of git accidents.

As a parting note, when rewriting history and pushing frequently `--force-with-lease` is your friend.


[^1]: Another good time to clean up the history is before merging, if we have added additional commits to address review comments. Don't merge commits that just exist to address comments, fix the original bad commits.

[^2]: Specifically I tinker a lot with the history, combining several of these techniques here, before I do the final interactive rebase to resolve all changes. This way I can change my mind halfway through without any trouble undoing anything.

[^3]: Think board games or [card games](https://mtg.fandom.com/wiki/Stack).

[^4]: Emacs users out there will probably be familiar with [the vastly superior Magit interface](https://magit.vc/manual/magit/Staging-and-Unstaging.html), which allows easy staging of individual lines.
