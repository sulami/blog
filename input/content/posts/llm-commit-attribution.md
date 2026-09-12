title = "About LLM Commit Attribution"
slug = "llm-commit-attribution"
created_at ="2026-09-12"
tags = ["staff"]
---

Most software places require at least one person other than the author complete
a code review prior to shipping code changes to production, the idea being two
pairs of eyes have a higher chance to catch issues than one. There's also an
undercurrent of not allowing any single actor to alter production code for
security reasons, especially once you get into certifications like PCI-DSS or
SOC 2.[^1] It is generally either frowned upon or more often than not outright
impossible to perform the sole review of a change if you are the one who
instructed an LLM to make the change. If you trust the LLM enough, one could
argue that the quality gating would be equivalent, but it does not protect
against bad (or compromised) actors, as the LLM is fully under your control and
cannot meaningfully object.

A topic related to this that has come up at `$WORK` is how to handle attribution
of changes to signify responsibility for a change and help determine the
appropriate level of code review required. git has a flexible system for
attribution with separated author, committer, and additional trailers, the
question was more how to use it correctly. I would like to propose a convention
here based on how a change was produced, specifically who should be attributed
as the author in git terminology.

- **If you write the code yourself or with limited aid from LLMs**, that is
  essentially the auto-complete case, comparable to something like early GitHub
  Copilot. In this case the LLM saved you some typing or research, but didn't
  make any significant decisions, and the code is checked and corrected by you
  line by line. Attribute the commit to yourself.
- **If an agent generated significant amounts of code autonomously**, for
  example a whole module or test suite, the most accurate attribution would be
  to use `Co-authored-by`, as now both you and the LLM are significant authors.
  You can pick ordering based on the ratio if you like. Some agents do this for
  all commits they generate, but I don't think this correct and mostly an LLM
  marketing feature.
- **If an agent generated essentially the entire change**, the LLM should be
  designated as the author. If you reviewed the change and potentially led the
  LLM make tweaks prior to opening a pull request, use the `Signed-off-by` or
  `Reviewed-by` trailer for yourself to indicate having reviewed the change. You
  can do the former using `-s` or `--signoff` when committing.

To set the author for a commit, use `--author=<author>` when committing. Setting
`Co-authored-by` requires doing so manually by adding the trailer in the commit
message.[^2] In all these cases, the committer should still be you, as the
commits are created with your tooling and (hopefully) signed with your key.

[^1]: As far as I know, none of them actually _require_ code reviews, but
use more vague language like "software development best practices", though
the [two-person rule](https://en.wikipedia.org/wiki/Two-person_rule) is a real
thing.

[^2]: For my fellow [Jujutsu](https://jj-vcs.dev) users, you can use `jj 
metaedit` to change author information, or potentially use
[conditional config](https://docs.jj-vcs.dev/latest/config/#conditional-variables)
to set the author and other trailers automatically when the appropriate.
