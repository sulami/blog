title = "Haskell Flood Fills"
timestamp = "2015-07-21"
tags = ["haskell"]
---
Over the past two days, I played a bit with the [hackthe.computer contest](https://hackthe.computer/). It is a contest with a couple of problems/tasks where you have one week to write up the best solutions in terms of speed. Part of the difficulty comes from the fact, that you push git commits to their server and do not get to to see the output of their test runs, which you only know if you program produces the correct output within a reasonable time frame (one minute per test?) or not. It is quite fun, also because the problems are nicely chosen, with a lot of data interpretation, but also a need for efficient algorithms, and they all have nice back stories to explain why we are dealing with seemingly random input.

The first problem is called *Bowie's in a maze*, which, without spoiling too much, involves David Bowie and a maze. Bowie is actually the villain here. To save our little brother, we have to find him in his maze. To accomplish this, we get an ASCII-map of the maze with our position and the position of our brother. The specific task is to find the shortest route in the least amount of time possible.

The naive first attempt was finding the shortest of all possible paths, but several of their test cases include really large mazes with few walls, if any at all (remember, I do not know). So this ran forever. From there, my mind obviously went to flood fills quite fast, because we save a lot of time, both by not building tons of overlapping paths, and, because a queue-based flood fill will return the shortest possible correct path before any longer solutions, using a lazy list of routes we can stop evaluating paths after the first one that reaches our brother.

I have written flood fills before, but mostly in Python, where the approach is quite a bit different. And because I could not really find a nice example of how to do flood fills in Haskell, I will post a general example here, just for you, fellow coder. So, here we go:

```haskell
-- First, we define a couple of types, for better readability. The grid
-- will be a line-wise list of strings, with every char being a cell.
type Grid = [String]
type Coord = (Int, Int)
type Path = [Coord]

-- So, we flood fill. This list can be lazily evaluated from the head and
-- returns branching paths in all directions that obey possibleWays and do
-- not overlap. flood is actually just a wrapper for fl.
flood :: Grid -> Coord -> [Path]
flood grid pos = fl grid [[pos]]
  where
    -- This might be a bit difficult to grasp, but this returns all paths
    -- ever evaluated and appends a copy of every path already evaluated
    -- that could continue somewhere for a single step without overlapping
    -- with any other path.
    fl :: Grid -> [Path] -> [Path]
    fl grid paths = paths ++ fl grid (concat (map (addRoutes grid paths) paths)

    -- This handles the replacing of a path with all its possible branches.
    -- It also has the context of all the other paths to prevent
    -- overlapping.
    addRoutes :: Grid -> [Path] -> Path -> [Path]
    addRoutes grid ps path = [ path ++ [p] | p <- possibleWays grid $ last path,
                                              not $ p `elem` (concat ps) ]

    -- All this does is, from a given point on the grid, look where we
    -- could go while obeying the rules of the game, like not leaving the
    -- grid (Index error!), only going one step (maybe diagonally?), etc.
    -- The actual code depends on what you want to do, but a list
    -- comprehension works well here, using the surrounding coordinates as
    -- inputs and filtering out the invalid ones.
    possibleWays :: Grid -> Coord -> Path
    possibleWays g (x,y) = [ (x1,y1) | .. ]

-- This is a simple little helper that checks if the last cell of a path is
-- the target, so we can filter out incomplete paths.
reachesTarget :: Grid -> Path -> Bool
reachesTarget g p = let (x,y) = last p in g !! y !! x == 'F'

-- This a a really basic example use of the functions above.
main = do grid <- fmap lines getContents          -- Read a map from stdin.
          let start = (0,0)      -- Start filling from the top-left corner.
          -- My example involves finding the length of shortest possible
          -- route, so we start flood filling lazily, and take the first
          -- complete path. Then we just print out its length, because
          -- paths themselves do not print nicely without a pretty printer.
          let path = head $ filter (reachesTarget grid) $ flood grid start
          print $ length path
```

You can use this code pretty much like this (I hereby license it under ISC), by just filling in `possibleWays`. Note that this can get a bit slow when the grid is large and contains few or no walls, because the list of paths to iterate over grows rather large. You can probably gain some additional performance by introducing parallelism via `par` and `pseq` if that becomes a problem.

PS\* If you are taking part in the competition, flood fills will not bring you far, the solution is a different one.
