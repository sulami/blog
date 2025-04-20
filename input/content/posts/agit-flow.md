title = "AGit-Flow"
slug = "agit-flow"
created_at = "2025-04-20"
tags = ["git"]
---

Making one-off contributions to projects hosted on GitHub is surprisingly
onerous, requiring first forking a repository into one's own account, then
cloning it to one's local machine, making the suggested change, pushing the
change, and finally opening a pull request. Afterwards, one is left with a
mostly useless fork repository.

When Linus Torvalds invented Git in 2005 to facilitate work on Linux, it was
designed around the idea of emailing patches directly to maintainers, and that
is still how Linux development works today. GitHub was launched in 2008 as a hub
for Git-managed projects, popularizing the pull request flow described above,
which has shaped today's de-facto collaboration model.

Bucking the trend, the more recent forge [sourcehut] relies on the `git
send-email` command to generate and send patches in email form, and using
mailing lists to manage change requests and issues. The interface is
(subjectively) less intuitive than GitHub's, and the setup required for sending
patches is involved enough to merit [its own page][git-send-email]. I like
snappy, JavaScript-free interfaces, but the whole thing feels a bit
unnecessarily elitist to me.

Another recent alternative is [AGit-Flow], a centralized Git workflow developed
at Alibaba, which enables contributing to a project directly from a local
checkout by using specific semantics of `git push`. It is [supported by
Gitea/Forgejo][support], and thus notably [Codeberg]. In its simplest form, a
contribution requires just

```bash
git push origin HEAD:refs/for/main/topic-branch
```

which will create a pull request against the `main` branch using the first new
commit's title and description, similar to GitHub's suggestion when opening a
new pull request. Those can be overridden with the `-o` option as well:

```bash
git push origin HEAD:refs/for/main \
  -o topic="topic-branch" \
  -o title="Title of the PR" \
  -o description="# The PR Description
This can be **any** markdown content.\n
- [x] Ok"
```

In comparison to the standard pull request flow, this dramatically cuts down on
ceremony, especially for smaller, one-off changes. I have some repositories of
my own on Codeberg, but it would be nice if GitHub as the current centre of OSS
development would chose to support this flow as well.

[git-send-email]: https://git-send-email.io
[sourcehut]: https://sourcehut.org
[AGit-Flow]: https://git-repo.info/en/2020/03/agit-flow-and-git-repo/
[support]: https://codename.codeberg.page/@main/docs/latest/user/agit-support/
[Codeberg]: https://codeberg.org/


