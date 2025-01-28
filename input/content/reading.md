title = "Recommended Reads"
slug = "reading"
kind = "page"

---

A list of books I recommend for software engineers, along with some reading 
instructions.

## Technical

- [A Philosophy of Software Design - John Ousterhout](https://www.amazon.co.jp/-/en/John-Ousterhout/dp/1732102201): Great introduction to all the unwritten rules around sustainable software engineering, I especially like it for its treatment of effective abstractions. Good first read, pretty short as well. If you’re well past senior level, this might be a bit basic in places. Read cover to cover.
- [Building Secure & Reliable Systems - Various Googlers](https://sre.google/books/): Delivers mostly on the secure part, and the only (good) book I’ve read that talks about security by design. Should probably be required reading for engineers. Covers a lot of the same topics as the SRE book as well, but in more depth. Pick and choose interesting chapters and keep as a reference. Parts 1 and 2 are good, part 3 can be skipped, parts 4 and 5 are interesting to senior+ engineers and managers. (Free to read on the website)
- [Designing Data-Intensive Applications - Martin Kleppmann](https://dataintensive.net/): Fantastic overview of the theory behind building scalable systems. Think distributed systems, consistency and ordering guarantees, partitioning, and data processing pipelines. A bit of a dense read at times. Skim for interesting sections and use as a reference.
- [Site Reliability Engineering - Various Googlers](https://sre.google/books/): Outlines the SRE model at Google, and their techniques. Interesting for the sections on SLOs, monitoring & alerting, and incident management. A friend of mine actually contributed to this one. Pick and choose chapters that seem interesting. (Free to read on the website)
- [Working Effectively with Legacy Code - Michael Feathers](https://www.oreilly.com/library/view/working-effectively-with/0131177052/): One of those massive tomes similar to Uncle Bob’s (also with his foreword). Has a lot of concrete concepts and techniques related to dealing with legacy code. [Read outside-in](/posts/how-to-read-a-book/) and use as a reference.
- [Crafting Interpreters - Robert Nystrom](https://craftinginterpreters.com/): A work book that guides you through building first an interpreter, and then a compiler for a new programming language. I personally found it fun, but also useful for understanding why languages look and work the way they do and what is happening behind the scenes. Work through in order, though you can choose different implementation languages than the author. I did the first half in Clojure and the second in Rust. (Free to read on the website)
- [Rust Atomics and Locks - Mara Bos](https://marabos.nl/atomics/): A deep dive into concurrency primitives in Rust. Honestly not useful without at least a basic level in the language. More broadly applicable though when it comes to thinking about concurrency on a low level, and the performance implications of different approaches. Read either cover to cover or individual chapters. (Free to read on the website)

## Organizational

- [Staff Engineer - Will Larson](https://staffeng.com/book): Compiled from his blog posts on [lethain.com](https://lethain.com), a book that outlines the role of the staff engineer. Best introduction for senior engineers interested in progressing on the technical track. Easy read and lots of actionable advice. Read cover to cover.
- [The Staff Engineer’s Path - Tanya Reilly](https://www.oreilly.com/library/view/the-staff-engineers/9781098118723/): The next book to read for staff engineers after Will Larson’s book. Considerably more abstract, and requires some work to get value out of. Probably read cover to cover, I’d actually recommend reading it as a group and discussing as you go along.
- [An Elegant Puzzle - Will Larson](https://lethain.com/elegant-puzzle/): Similar to Staff Engineer, an intro book compiled from blog posts. Interested in becoming a manager, or just became one? This is the first book to read. Can also be interesting for staff engineers interested in broadening their horizon. Read cover to cover.
- [Team Topologies - Matthew Skelton & Manuel Pais](https://teamtopologies.com/): A modern take on laying out different kinds of teams for different functions and modes of operation. Mostly interesting to managers or executives. I actually recommend you don’t read the book, but instead [watch the author’s talk](https://www.youtube.com/watch?v=haejb5rzKsM&pp=ygUPdGVhbSB0b3BvbG9naWVz) and supplement that with some summaries on the internet. The book is way too long for what it has to say, but might be useful for clarification after that.
- [Measure What Matters - John Doerr](https://www.whatmatters.com/the-book): This is OKRs - the book. I don’t really buy into it, and think OKRs are fraught with problems, but if your organization uses OKRs you probably still want to read this to be able to contribute. [Read outside-in](/posts/how-to-read-a-book/).

## Cross-Functional

- [Thinking in Systems: A Primer - Donella Meadows](https://en.wikipedia.org/wiki/Thinking_In_Systems%3A_A_Primer): Referenced in some of the books in the organizational section, an introduction to systems thinking. A bit verbose, but makes sure you can follow. Also a bit abstract when it comes to concrete applications. Read either cover to cover or skim through.
- [How to Read a Book - Mortimer J. Adler](https://en.wikipedia.org/wiki/How_to_Read_a_Book): Describes a technique to read books “outside-in,” instead of front to back. Provides more value earlier and allows to extract most of the value from a book in a fraction of the time. Ironically read this one cover to cover.
