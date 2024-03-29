title = "Writing for Reasons"
timestamp = "2020-11-08"
---
This year, I have been writing more than even before over. In this article, I would like to discuss some of the reasons for writing and provide some thoughts on each.


# Writing to Remember

This is probably the most obvious reason to write for a lot of people. Having written down a piece of information, you can come back later and recall it. Historical context can be invaluable for decision making, and often covers information that is not readily available anymore.

The key here is being able to find notes later on. Paper-based ones can be sorted by topic or chronologically, digital ones can be searched for.[^1] Formats can be useful here too, for example by supporting embedded code blocks or graphics.


# Writing to Solve Problems

Early this year, before the pandemic hit Europe, I saw Paulus Esterhazy's talk *[Angels Singing: Writing for Programmers](https://www.youtube.com/watch?v=T7-2DW-KDV4&t=1429s)* at [clojureD](https://clojured.de/). It contained this great quote of Milton Friedman:

> If you cannot state a proposition clearly and unambiguously, you do not understand it.

In [another talk](https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/HammockDrivenDev.md), Rich Hickey explained his notion of using notes as an extension of his working memory:

> So we have a problem, in general, because we're just being asked to write software that's more and more complex as time goes by. And we know there's a 7 +/- 2 sort of working memory limit and as smart as any of us are, we all suffer from the same limit but the problems that we are called upon to solve are much bigger than that. So what do we do if we can't think the whole thing in our head at the same time? How can we work on a problem with more than nine components. What I'm going to recommend is that you write all the bits down.
>
> [&#x2026;]
>
> But if we look at the 7 +/- 2 thing, we could say we can juggle seven to nine balls but if you can imagine having an assistant who every now and then can take one of those out and put a different color in and you can juggle balls of 20 different colors at the same time as long as there are only nine in the air at any one point in time. And that's what you're doing, you're going to sort of look around at all these pieces and shift arbitrary shapes of seven into your head at different points in time.

Writing everything down allows digging deep into details and going off on tangents, and then returning to other aspects. As an added bonus, these notes can be useful in the future as well, if archived properly. I found [org-mode](https://orgmode.org/features.html) outlines incredibly powerful for this purpose, with their foldable, tree-like structure that allows nesting sub-problems.


# Writing to Make Decisions

Writing is invaluable for decision making. Not only does it aid the decision process (see above), it also allows returning to a decision later and reviewing it.

[Architecture decision records (ADRs)](https://github.com/joelparkerhenderson/architecture_decision_record) are a tool established just for this purpose. The exact formats vary, and the details do not matter too much, but here are a few key points I consider essential:

-   The motivation for the decision
-   The constraints involved
-   The alternatives to consider and their respective tradeoffs

All of these are useful in several ways: they force you to acknowledge the components of the decision, make it simple to get an opinion on the matter from someone else, and also allow you to review the (potentially bad) decision later on.

There is one more point: the conclusion. This is easy to forget, because once a conclusion is reached, no one wants to spend time writing it down. But if you do not write it down, the document does not tell the whole story if reviewed in the future.


# Writing to Develop Ideas

This year I have seen a lot of people writing about Sönke Ahrens' [*How to Take Smart Notes*](https://takesmartnotes.com/), which is about taking notes as a means to develop long form writing. It popularised the idea of the *Zettelkasten*, a physical or virtual box of notes which reference each other to build an information network.

While I found the book quite interesting, I would not recommend it to everyone due to the significant organisation overhead involved.[^2]

That being said, I believe that if you have a digital system which can provide automatic back-links to avoid the exponentially growing amount of manual maintenance required, there is little harm in linking notes.[^3] At the very least it will make it easier to find a note, and maybe it can aid the thinking process by exposing previously unseen connections between concepts.


# Writing to Communicate

This very article was written expressively to communicate information, and as such required some extra work for it to be effective.

The most important factor when writing for communication is the target audience. It dictates the format to use, and which prior knowledge can be assumed. Maximising information density by being as concise as possible is important to avoid wasting the reader's time.

As an added difficulty, when writing something to be published you need to get it right the first time, there is no channel for discussing follow-up questions. The old adage in writing is "writing is rewriting", and I very much believe that to be true in this case. Write an outline, then a first draft, then keep reading and revising it until it is just right. Maybe show it to someone you trust for feedback.

I personally also like to leave a draft and come back a few weeks later. This way I always have a few drafts for new articles ready for revision, until I feel that one is ready for publishing.


[^1]: As an aside, I find paper notebooks really clumsy in this regard. They make a decent "staging area" to quickly capture information, but are terrible to find anything in unless you take care to maintain an index. Index cards can at least be reordered instead.

[^2]: I think researchers and writers can gain a lot from this method, but others not so much. Of course, if you want to read the book, feel free to do so. It is an interesting read, and I wouldn't call it overrated by any means.

[^3]: [Roam](https://roamresearch.com/), [Notion](https://notion.so), [Obsidian](https://obsidian.md/), and [many others](https://github.com/Kungsgeten/org-brain) can do so.
