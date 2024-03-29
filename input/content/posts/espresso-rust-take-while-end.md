title = "Find Out If .take_while() Reached The End"
slug = "espresso-rust-take-while-end"
timestamp = "2022-12-09"
tags = ["espresso", "rust"]
---
In yesterday's Advent of Code (**mild spoilers ahead**), there was a section where one had to figure out how far the elves can see, based on the height of a line of trees in that direction. The answer was to be given in the number of trees they see.

It is a pretty simple thing to do in most high-level languages, assuming we have an iterator over tree heights, just keep going until we reach a tree that's tall enough so that it blocks the elves' view.

```rust
let count = trees.take_while(|&t| *t < too_high).count();
```

Because `.take_while()` stops when it encounters the first tree that's too tall, we have to actually add one to this result, given that we still see that tall tree. But on the other hand if there are no tall trees, we will reach the edge of the forest, i.e. the iterator, at which point the `.count()` will be accurate without adding one, as there is no next tree to see.[^1]

The neatest solution for this I could come with hooks into the predicate function to signal if we ever rejected a tree before the iterator ran dry.

```rust
let mut extra_tree = 0;
let count = trees
    .take_while(|&t| {
        if *t < too_high {
            true
        } else {
            extra_tree = 1;
            false
        }
    })
    .count();
count + extra_tree
```


[^1]: AoC is full of these kinds of subtle difficulties, which is great.
