title = "A Fuzzy Finder in Haskell"
timestamp = "2016-06-24"
tags = ["haskell"]
---
This is a response to *[FuzzyFinder - in 10 lines of Python](http://blog.amjith.com/fuzzyfinder-in-10-lines-of-python)*, which was posted to reddit yesterday.

Following the blogpost linked above, I decided to write my own fuzzy finder in Haskell, because that is the language I am currently learning, and think has great potential. It is also a more interesting [exercise](https://github.com/sulami/spielwiese/tree/master/hEuler) than [Project Euler](https://projecteuler.net/).

Just in case you have not read the original post and do not know what a fuzzy finder is, it is a mechanism to filter and sort a list of strings by searching for substrings. It is often used in text editors like vim or Sublime Text, where you can just type in *"accmanba"* and they will open up `account_management_backend.py` for you. As you can see, it makes switching between more than two files in a project much easier and faster.

Amjith wrote his finder using regular expressions, which are part of the Python standard library and can be compiled to be reasonably fast very easily. Sadly, Haskell has no implementation of regular expressions in its standard library, and I did not want to use third-party ones just for this. But as it turns out, we do not even need them, because the task is so simple and Haskell's string manipulation capabilities are incredible, so that we can solve this by implementing the search algorithm ourselves and still achieve good performance.

So, let us get on to some actual code. The most interesting part here is the matching algorithm:

```haskell
partOf :: Char -> [Char] -> Int -> (Bool, Int)
partOf _ []     r = (False, 0)
partOf c (x:xs) r |    c == x = (True, r + 1)
                  | otherwise = partOf c xs $ r + 1

match :: [Char] -> ([Char], [Char]) -> (Bool, [Int])
match i s = match' i (snd s) []
  where
    match' :: [Char] -> [Char] -> [Int] -> (Bool, [Int])
    match' []     _ r = (True, r)
    match' (x:xs) s r | fst check = match' xs (drop used s) $ r ++ [used]
                      | otherwise = (False, r)
      where
        used = snd check
        check = partOf x s 0
```

I know this is not really optimized for readability and especially not if you do not know Haskell, but stay with me, it is quite simple. Before we compare the input, we map all the possible solutions to lowercase and store them in a tuple like `("String", "string")`. This way, we can compare against the lowercase version and return the properly capitalized one later on. All we do then is check for each possible solution if each character of the input string appears in order in the solution. If so, we add it to a list along with some data, specifically the position of the first match and the distance between the first and last matched character in the solution. This is the same Amjith did for sorting. All this data gets returned in a big list of tuples with both versions of the solutions and the match data. It is not pretty, but it works.

The one function the module actually exports is `fuzzyFinder`:

```haskell
fuzzyFinder :: [Char] -> [[Char]] -> [[Char]]
fuzzyFinder input list = map fst . map snd $ sort combo
  where
    combo = zip (zip ((map sum . map tail) scores) (map head scores)) matches
    scores = map snd $ map (match input) matches
    matches = filter (fst . match input) $ prepInput list
```

All this function does is build the tuple list with the lowercase versions, toss it into the match function and filter out the correct versions from the matches that came back, ordering them by the match data in the same way Amjith did it. There are just a couple of extra lines that I omitted here because they are not important, but you can find the complete source on [Github](https://github.com/sulami/spielwiese/tree/master/hFuzzyFinder).

Now you might say, this cannot be fast, it is iterating through all this stuff and with big enough input it will take forever to present results. Let me show you this:

```haskell
readL :: String -> [String]
readL a = read $ "[" ++ a ++ "]"

main = do args <- getArgs
          if length args == 2 then do
            list <- readFile $ args !! 1
            print $ fuzzyFinder (args !! 0) $ readL list
          else
            putStrLn "Wrong number of args"
```

This is a small program that takes two arguments, the query string and a file path of a list of possible solutions and performs the actions outlined above. Using a 2.2GHz Core 2 Duo, because I am using my laptop, and a 46K list containing over 5000 words (Thanks, Project Euler), this happens:

    λ time ./interactive roro ../hEuler/022.input
    ["ROBERTO","RODRIGO","ROSARIO","GREGORIO","RIGOBERTO"]
    ./interactive roro ../hEuler/022.input  0.08s user 0.00s system 96% cpu 0.079 total

The execution time goes up to 0.09 seconds when printing out really many names, but that is caused by the fact that we have to print out 100 lines or more to the console, which also takes time. But in my opinion, this is more than fast enough for auto-completion, which is the main use for fuzzy finders.

So overall, I am really happy with how this turned out. I was able to write this on one morning despite still learning the language. It is reasonably concise despite the fact that I did not use regular expressions but searched the strings manually and also quite fast.
