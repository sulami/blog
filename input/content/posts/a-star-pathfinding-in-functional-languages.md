title = "A* Pathfinding in Functional Languages"
slug = "a-star-pathfinding-in-functional-languages"
timestamp = "2015-07-23"
tags = ["haskell"]
---
&#x2026; like, you guessed it, Haskell. But this should translate to Lisp quite well. [A\* (A star)](https://en.wikipedia.org/wiki/A*_search_algorithm) is a graph traversal algorithm that is commonly used for pathfinding in video games, and probably also things like navigational software. It is an extension to [Dijkstra's algorithm](https://en.wikipedia.org/wiki/Dtra%27s_algorithm). If you do not know how they work, there is a really nice interactive explanation over on [Red Blob Games](http://www.redblobgames.com/pathfinding/a-star/introduction.html). There are a lot of examples around how to implement it in imperative languages, including the pseudo code on Wikipedia, but it is actually difficult to find an example for functional programming languages, where the approach differs a bit. As you will see, the general structure will be very close to the one used for [breadth-first traversal](https://github.com/sulami/spielwiese/blob/master/hUtil/BTree.hs#L69), the reason for this being that both algorithms essentially are breadth-first traversal with a weighting function that decides where to traverse deeper. With all paths having equal weights, they will behave exactly like breadth-first.

So I wrote up [a small something](https://github.com/sulami/spielwiese/tree/master/astar) that uses this algorithm in Haskell. It operates on ASCII mazes and draws the shortest possible path through them in (sort of) reasonable time. It also features experimental multi-threading support, although I am not quite sure yet just how big the performance boost is by using several cores. The results look like this, the asterisks (or stars&#x2026;) show the path:

```python
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX** X
X   X  *************X***********    X           X  *********        X*  X
X   X  *XXXXXXXXX  *X*  XXXXX  *XXXXXXXXX   XXXXX  *XXXXX  *XXXXX   X*  X
X      *********X  ***  X   X  *******  X          *X   X  *X       X*  X
XXXXXXXXX   X  *XXXXXXXXX   XXXXXXXXX*  XXXXX   X  *X   X  *XXXXXXXXX*  X
X  *****X   X  *************X  *******  X   X   X  *X   X  *******  X*  X
X  *X  *XXXXXXXXXXXXX   X  *X  *XXXXXXXXX   XXXXX  *X   XXXXXXXXX*  X*  X
X  *X  *************X   X  *X  *****X           X  *********X  ***  X*  X
X  *XXXXXXXXXXXXX  *XXXXX  *XXXXX  *X   XXXXX   XXXXXXXXX  *X  *XXXXX*  X
X  *********X      *X   X  ***  X  *X       X***********X  *X  *******  X
X   XXXXX  *XXXXX  *X   XXXXX*  X  *XXXXXXXXX*  X   X  *X  *XXXXXXXXXXXXX
X       X  *****X  *X   X*****  X  *****X  ***  X   X  *X  *****X       X
XXXXXXXXXXXXX  *X  *X   X*  XXXXXXXXX  *X  *XXXXX   X  *XXXXX  *XXXXX   X
X ********  X  *X  *******  X       X  *X  *****X   X  *    X  *******  X
X * XXXXX*  X  *XXXXXXXXX   XXXXX   X  *XXXXX  *XXXXX  *XXXXXXXXXXXXX*  X
X * X  ***  X  *******  X           X  ***  X  *X   X  *************X*  X
X * X  *XXXXXXXXX   X*  XXXXX   XXXXXXXXX*  X  *X   XXXXXXXXXXXXX  *X*  X
X * X  *********X   X*  X   X   X  *******  X  *************X   X  ***  X
X * XXXXXXXXX  *X   X*  X   XXXXX  *XXXXXXXXX   XXXXXXXXX  *X   XXXXXXXXX
X * X       X  *X   X*          X  *********X   X*******X  *******      X
X * X   XXXXX  *XXXXX*  XXXXX   XXXXXXXXX  *XXXXX*  X  *XXXXXXXXX*  X   X
X * X          *******  X           X      *******  X  ***********  X   X
X * XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
```

A maze of this size can be solved in about a quarter of a second on my 2008 laptop. So let us find out, how one would implement this sort of path finding using a purely functional language like Haskell.

The core of this algorithm is essentially a [flood fill](https://en.wikipedia.org/wiki/Flood_fill). But where a flood fill expands in all directions equally (if it is a queue-based one), A Star reorders the cells to fill after every filled cell using a cost function, and always fills the cheapest cell available. As such, our function needs to take a couple of arguments to work, the grid on which it works, the start and finish positions on the grid (we will need the target to determine costs), a function that can determine the valid cells a path could advance on, and a cost function that can determine the cost of a path.

The grid, start and finish positions are quite easy, in our case we have a two-dimensional grid that consists of `Char=s, and positions on it look like =(x,y)`. The cost function has to follow a couple of basic rules. If you have a look at the Wikipedia page I linked at the very top, there is a part that explains that the cost of a path consists of the sum of its length, and an estimate of the distance between its end and the target. It should also not overestimate the minimal distance to the target. In our case, this is very simple. I define the rules of motion to be that we can only move in four directions, and not into or through walls (duh). This means our example cost function looks like this:

```haskell
cost :: Coord -> Path -> Int
cost fin path = let l = last path in (length path - 1) + (dist l fin)
  where
    dist :: Coord -> Coord -> Int
    dist (x0,y0) (x1,y1) = abs (x0-x1) + abs (y0-y1)
```

This is one of the functions we will be passing to the actual path finding function. The other one will be the one that determines which cells are valid options. This follows a couple of basic rules that I annotated here:

```haskell
possibleWays :: Grid -> Coord -> Path
possibleWays g (x,y) = [ (x1,y1) | y1 <- [(y-1)..(y+1)],    -- Build a set of y
                                    y1 >= 0,                -- and x coordinates
                                    y1 < length g,          -- within the grid and
                                    x1 <- [(x-1)..(x+1)],   -- a maximum distance
                                    x1 >= 0,                -- of one cell in any
                                    x1 < length (g !! y1),  -- direction.
                                    x-x1 == 0 || y-y1 == 0, -- Only one step in one direction.
                                    g !! y1 !! x1 /= 'X' ]  -- Not into a wall.
```

As you can notice, the function takes the grid and a position, and determines which cell can be entered on the grid from withing the position provided. This function has to supplied, because, just like the cost function, its specifics depend on the rules of the game. If you, for example, want to be able to move diagonally, you can adjust this function and the cost function without touching the path finding algorithm itself.

The only thing left is the path finding itself. I will split this one into parts so it is easier to understand. A complete source with example including an ASCII parser and printer is on [GitHub](https://github.com/sulami/spielwiese/tree/master/astar). While it might look intimidating, the core algorithm is actually quite simple. It sorts all paths in a list by cost, and replaces the cheapest path by all paths that could extend this path while obeying the rules of `possibleWays` and not adding a cell that is already part of another path. The comparison is done using a simple zip:

```haskell
fl :: Grid -> Coord -> PossibleWaysFun -> CostFun -> [Path] -> [Path]
fl grid fin pwf cf paths
  | any (\p -> last p == fin) paths = filter (\p -> last p == fin) paths
  | otherwise = let best = snd $ minimum $ zip (map (cf fin) paths) paths
                    pb = addRoutes grid paths best pwf
                in fl grid fin pwf cf $ filter (/= best) paths ++ pb
```

A path is replaced by all *new*, valid paths using

```haskell
addRoutes :: Grid -> [Path] -> Path -> PossibleWaysFun -> [Path]
addRoutes grid ps path pwf = let cps = concat ps in
  [ path ++ [p] | p <- filter (`notElem` cps) $ pwf grid $ last path ]
```

Do not worry about the funny types, they are just for clarification and declarations can be found in the complete source. The list comprehension ensures we source the possible replacements and filter out the ones that would cross existing paths. If a path cannot go anywhere new, it therefore gets removed altogether, and a new cheapest path will be chosen the next iteration.

To finish this off, we just package this into a nice clean wrapper like this:

```haskell
flood :: Grid -> Coord -> Coord -> PossibleWaysFun -> CostFun -> Path
flood grid fin pos pwf cf = head $ fl grid fin pwf cf [[pos]]
```

And we get a single function that takes all the relevant data and returns one path. Note how the initial set of paths consists of the starting position, which is in itself a path of length 0. Also note how this will fail horribly if there is no possible path. You could transform this quite easily using `Maybe` to accommodate for that. And that is already pretty much it.
