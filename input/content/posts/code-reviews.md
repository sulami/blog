title = "Traps to Avoid When Reviewing Code Changes"
timestamp = "2021-01-18"
---
Reviewing code changes is an underappreciated art. It is part of most software engineers' daily routine, but as an industry we do little towards developing it as a skill, even though it contributes directly to the quality of the software we produce.


# The LGTM Trap

Characterised by the eponymous review comment,[^1] this trap can have different root causes, all of them resulting in the rubber-stamping of a bad pull request.

First of all, conducting proper code reviews is difficult and mentally exhausting. It is important to take breaks when conducting long or consecutive reviews.[^2] Resist the temptation to just "get something in" because it has been open for a while, or because someone else is blocked. Avoid including anything that you already know will need fixing up later, this road leads to [broken windows](https://blog.codinghorror.com/the-broken-window-theory/). This also means your comment-change-test cycle should be as fast as possible to encourage fixing even the smallest issues before merging.

If a critical issue makes it past your review, you should investigate how it got missed, and what you can do to catch similar issues in the future. This way you can build your own checklist to use during reviews. You can also ask someone you trust to conduct a supplementary review, or even try pairing with them on some reviews.


# The Human Linter Trap

Engineering time is expensive, and the focus required for good reviews is hard to maintain, so minimising the time required for a review is key. This is why we should avoid automatable tasks in code reviews. Prime examples include linting and enforcing a style guide. A [pre-commit hook](https://www.git-scm.com/docs/githooks) or a CI job can do either of these much more efficiently than a human reviewer ever could.

Beyond the efficiency gains, this also avoids the clutter resulting from many small comments pointing out typos, bad indentation, or non-idiomatic code, and lets both the reviewer and the author focus on more important issues.


# The Implementation Trap

Tests can not only be useful to ensure correctness of our code,[^3] they can also help us during review. Tests exercise the interface of our code, ideally without giving us too much of an idea of the implementation. As a general rule, you want to be reviewing changes from the outside in.

This forces you to actually understand the test code, assert that the checks performed actually match the contract you expect the code to abide by, and catch potential holes in test coverage. It also allows you to judge the interface provided, as awkward tests often hint at sub-optimally factored code.


# The Iceberg Trap

^7&frasl;<sub>8</sub> ths of an iceberg are famously below the water line and functionally invisible. Similarly some of the most important parts to pay attention to during reviews are not visible in the diff. This can range from introducing some avoidable duplication because the author was not aware of existing code with the same functionality, all the way to production outages because a remote piece of code made an assumption that does not hold anymore.[^4]

It can be helpful to checkout the change locally and look at it in the context of the entire code base instead of in isolation. Asking others familiar with the code base or related ones to have a cursory look can also uncover a wide range of problems quickly.


# The Rube Goldberg Trap

Just because you can, it does not mean you should. And sometimes there is a better solution than the one proposed.

To review a change, it is important to agree on the problem to solve. Ask the author to supply a problem statement if it is not presented in the change. Only once you understand the problem statement you can evaluate the quality of a solution. The solution could be over- or under-engineered, implemented in the wrong place, or you could even disagree with the problem statement altogether.


[^1]: Meaning: *Looks good to me.* Of course a code change can also just be good as is, even after careful review.

[^2]: [Pomodoros](https://en.wikipedia.org/wiki/Pomodoro_Technique) can work well here.

[^3]: and aid the design of our system

[^4]: While this is a sign of bad system architecture, realistically we all have to work with existing code, and the legacy that comes with that.
