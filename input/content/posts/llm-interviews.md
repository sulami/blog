title = "LLMs Do Not Break Interviews"
slug = "llm-interviews"
tags = ["staff"]
created_at ="2025-02-13"

---

It is that time of the year again, new headcount is approved, you are going to
hire some software engineers. A meeting is called to refresh the interview
process. Someone asks _"but how are we going to prevent candidates from just
using ChatGPT to pass the interview"_ and everyone murmurs in assent.

---

If you are worried about software engineering candidates using LLMs to pass your
technical interviews, one of these is true:

1. The job you are hiring for could be done by an LLM. You should probably ask
   yourself why you are hiring a human to begin with, if the minimum viable
   candidate is a human clipboard.[^1]
1. Your interview process does not test for the skills you actually require.

The second case is likely more common, you have the need for a (human) software
engineer, and you want them to do something in your organization, but the test
you devised to select suitable candidates can be passed by LLMs. I have come to
believe this is a result of the industry over-indexing on preexisting
experience[^2] instead of the more valuable ability to learn, adapt, and make
decisions with imperfect information.

LLMs are trivially able to reproduce more knowledge in a plausible-looking shape
than any human, but they struggle with logical reasoning and decision
making.[^3] Not only is software engineering itself a vast and rapidly growing
field, your organizational context is likely also changing all the time. In this
environment, software engineers constantly make decisions, big and small, and we
want them to make the best decisions possible lest we pay for their mistakes.

The good news is that it is not difficult to test for these abilities, provided
you already have skilled software engineers on your payroll.[^4]
Live-programming and LeetCode-style exercises have been rightfully stigmatized
at this point, but a suitable take-home exercise can provide a great signal for
software engineering acumen, if done right. Suitable here meaning representative
of actual work, and not taking too much time for the candidate.[^5] The [Gilded
Rose](https://github.com/emilybache/GildedRose-Refactoring-Kata) is not perfect,
but a decent start. The point of the take-home is not for the submission to
stand on its own, but to provide a topic to conduct an interview around. 

Ask the candidate why they made the decisions they did. What else did they
consider doing? How did they deal with ambiguity in the prompt? What would they
have done differently if they had more time and had to ship this to production?
What questions would they have asked their product manager if they could? If the
parameters of the scenario were different, how would that affect their
decisions? You can use open-ended questions to see what they know and how they
can apply it to the problem, without providing them with an obvious checkbox to
tick.[^6]

This is where you find out if the candidate is able to make reasonable decisions
when left to their own devices, and where you easily root out ones who fed the
exercise prompt into ChatGPT and called it a day. To some, LLMs are a useful
tool to produce code, just as snippets and auto-completion are, but we were
never testing for typing speed.

[^1]: I have to credit [Namanyay
    Goel](https://nmn.gl/blog/ai-illiterate-programmers) for this fantastic
    expression.
[^2]: Some broad knowledge is good to have, like "[queues don't fix
    overload](https://ferd.ca/queues-don-t-fix-overload.html)", while
    programming trivia like "how to invert a binary tree" is mostly useless, as
    it can be looked up on demand.
[^3]: Recent "reasoning LLMs" are no exception, they effectively just talk to
    themselves to produce a plausible-sounding chain of thought, instead of
    using logical deduction like humans do.
[^4]: One of the hard truths of hiring is that it's really difficult to hire
    better people than you already have. When it comes to skilled individuals,
    in many ways it takes ones to know one, or maybe to appreciate one.
[^5]: If it takes the candidate more than two hours, it would be a real class
    act to compensate them for their time, if you can somehow square that with
    your finance department.
[^6]: If you ask your candidate if they know about X, of course they will start
    talking about X, but the question is, would they think of X if not prompted?
    X could be all kinds of things, observability, horizontal scalability, user
    experience, you name it.
