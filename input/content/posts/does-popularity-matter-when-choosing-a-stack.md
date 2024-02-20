title = "Does popularity matter when choosing a stack?"
slug = "does-popularity-matter-when-choosing-a-stack"
timestamp = "2018-06-29"
---
Recently there has been a lot of turmoil around [Vue.js](https://github.com/vuejs/vue) [reaching the same number of stars on GitHub](https://hasvuepassedreactyet.surge.sh) as [React](https://github.com/facebook/react). While stars on GitHub are far from a reliable indicator of actual popularity and size of user base, it does raise the question whether these should actually matter when choosing a stack for a new piece of software.

The easy answer is that it should not really matter. Especially if you are working in a somewhat specialised environment or are under a lot of constraints, like low-power hardware or strict performance requirements, you will probably end up using a very domain-specific set of tools. A language like Kotlin is the recommended way forward if you are developing an android application, but looking at the [TIOBE index](https://www.tiobe.com/tiobe-index/)[^1], and still it only comes in at position 49 at the time of writing, so obviously popularity cannot matter more than platform constraints.

Another quite important factor is the already present repertoire of tools. Many tools, especially languages and large frameworks, like Vue.js and React, require rather large upfront learning efforts before they can be used efficiently. If you already know React inside out, why would you learn Vue, assuming it fundamentally does not enable you to do anything React does not? I am currently paying for my food by writing code in a primarily Python/Django environment, and sometimes I look over at the neat lawn the Ruby on Rails crowd seems to have, but it would not be wise of me to invest a sizeable amount of time to switch ecosystems without actually gaining much.[^2]

I can already hear you type your furious [tweets at me](https://twitter.com/_sulami_), claiming how popularity of course makes a difference. And you are not wrong, there are very valid arguments to be made.

Popularity does indeed matter when you chose a technology to use, for example if you need to add support for a new data format and shop around for libraries. Especially in the open-source world a larger user base means more eyes scanning the code for bugs, more potential contributors keeping the software updated and supplying it with new features, and better chances it will stay maintained overall. If you use a common set of softwares, chances are they can interact easily in some way, saving you time for integrating them. All of these are important factors which should be considered when making a voice of tooling.

In the end there is no single deciding factor to look at, like for example popularity, but instead a lot of different ones. It is crucial to recognise all of them and think about how to weight them in importance. Newer (and shiny) tools might offer significant gains, but come at the price of a smaller community, so less support and a higher risk of abandonment.


[^1]: I have to mention that I believe the TIOBE index to be a bit biased though, not that I was perfectly objective. The overall "winner" of 2013 is Transact-SQL, a language many programmers will not even have heard of.

[^2]: Apart from that Python is also larger and growing in comparison to Ruby. Ruby is probably not dying anytime soon, but it is definitely declining since the days when everything was Rails, much to my dismay.
