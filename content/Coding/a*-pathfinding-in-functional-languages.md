Title: A* Pathfinding in Functional Languages
Date: 2015-07-23
status: draft

... like, you guessed it, Haskell. But this should translate to Lisp quite
well. [A* (A star)][wiki] is a graph traversal algorithm that is commonly used for
pathfinding in video games, and probably also things like navigational
software. It is an extension to [Dijkstra's algorithm][da]. If you do not know
how they work, there is a really nice interactive explanation over on [Red Blob
Games][rbg]. There are a lot of examples around how to implement it in
imperative languages, including the pseudocode on Wikipedia, but it is actually
difficult to find an example for functional programming languages, where the
approach differs a bit. As you will see, the general structure will be very
close to the one used for [breadth-first traversal][bft], the reason for this
being that both algorithms essentially are breadth-first traversal with a
weighting function that decides where to traverse deeper. With all paths having
equal weights, they will behave exactly like breadth-first.

[wiki]: https://en.wikipedia.org/wiki/A*_search_algorithm
[da]: https://en.wikipedia.org/wiki/Dtra%27s_algorithm
[rbg]: http://www.redblobgames.com/pathfinding/a-star/introduction.html
[bft]: https://github.com/sulami/spielwiese/blob/master/hUtil/BTree.hs#L69

