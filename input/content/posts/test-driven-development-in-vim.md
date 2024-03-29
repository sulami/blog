title = "Test-Driven Development in Vim"
timestamp = "2014-05-05"
tags = ["vim"]
---
[Test-driven development (TDD)](https://en.wikipedia.org/wiki/Test-driven_development) is a way of developing software by using tests over and over again. When I first was introduced to this concept, I thought, automated tests are a nice idea, why not? Then I read a book about TDD, which proceeded to explain how you start your project by writing a test that (obviously) fails, then start your actual project, and test again. At this point in time, we have a something that returns a default webpage, or zero. Not even printing "Hello World" or "Success!". The next step is to write another test, which checks for some actual content or functionality. After this test fails (which by the way is important, as it shows that the test will hopefully not produce false positives), we write the piece of content or functionality.

In the beginning, this way of writing code seems (and is) incredibly slow, and I find myself skipping the first five tests until I have something that actually produces useful output. This is sort of okay, I do not think it will hurt my code in any major way. But after those initial skipped tests, say 10 minutes into the project, I start to religiously write tests for everything that happens, which is way easier than implementing proper tests for all the different stages and levels when you wrote code for several hours and start wasting time by manually testing changes. I think, everything that takes more than 30 minutes to code benefits from TDD.

But let's have a look at some actual TDD using vim and C. C is not often written using TDD, but giving usually rather poor debugging output compared to interpreted languages, TDD is even more useful, as you will instantaneously see when a change breaks any functionality. To make testing easier, we will define a keyboard mapping to start our tests from within vim.

```haskell
map <Leader>t :!make -B tests && ./tests<CR>
```

This mapping will call make in a shell (which means you have to have make installed), compile our *tests.c* file and run it. Depending on which language you are using, which kind of application you are developing and how your tests work, you might want to change this a bit. I am testing a library I am writing, which gets included in *tests.c*. The basic structure looks like this:

```C
/* Tests for my library */
#include <stdio.h>
#include <assert.h>
#include "mylib.h"

#define run_test(fn_name) \
    printf("%s\n", #fn_name); \
    fn_name();

void test_example_test() {
    int test = fancy_function(42);
    assert(test == 15);
}

int main() {
    printf("\nRunning tests:\n");
    run_test(test_example_test);
    printf("\n => All tests successful!\n");
    return(0);
}
```

We include *stdio* for printing and *assert* for the actual testing. The way assert works is if the statement assert gets passed is false, which means it can also be a function call, assert stops the whole program and prints out the line which produced the error. If everything works as intended, it prints all the tests it ran, prompts for enter and returns us to vim. An important part is to only delete old tests, when the condition they test for is no longer wanted, so you always test for basic functionality as well as the final results.
