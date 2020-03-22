---
title: Why You Need Fuzz Testing
---

Unittesting is an important part of building reliable software, although by far
not the only one. But many programmers misunderstand the purpose of unittests.
Classic unittests have two purposes: They force you to develop a spec and
enforce it, and they can protect you against regression when refactoring or
otherwise changing your code. Contrary to what some believe, they do not ensure
correctness of your code.

Static unittests have one major flaw: The programmer has to write them. Any
function working on a list is usually tested using some small examples and an
empty list to make sure nothing breaks. This usually means you have to write
3-5 test cases per function to cover basic functionality. Not only is this a
lot of work, it is also error-prone, because some cases are easily forgotten,
especially because the programmer writing the tests for his code has a set of
preconceived notions about how his code might be used.

But when writing any meaningful software, you usually have to deal with all
kinds of data from outside, and a lot of that data is to be mistrusted. There
might be changes in APIs, software errors somewhere else, or even a malicious
attacker trying to breach your system, and all that data could look anything
but the way you expect it to. This is why unittests should be used to employ a
contract that the tested code has to follow, and fuzz testing can help you do
just that.

Haskell with its amazing type system has a very nice way to do this. Have a
look at this test:

```haskell
describe "the finder" $
  it "only returns proper superstrings of the search term" $
    forAll nonNull $ \x -> property $
      \y -> let rv = finder x y
            in rv `shouldBe` filter (isSubsequenceOf x) rv
```

This code is part of a fuzzy finder I am toying with, and it ensures that a
basic fact about it is always true, the results should always be proper
superstrings of the search term, and the search term in turn a proper substring
of all results. I could write a bunch of test cases including input like
`"abcdef"` or `"123"` and supply a list of possible results, then evaluate
myself what the result should look like and just assert that the results are
equal.

But that would not only result in a considerable increase in code, easily
quadrupling the code above, it would also not cover all possibilities.
Haskell's type system ensures that the finder only gets used with strings and
lists of strings, but strings can have many forms. Because all of the input is
coming from the user, there might be punctuation in there, spaces, maybe
escaped, maybe Unicode symbols, who knows. That is why I instruct the test
suite to just generate random strings and lists of strings to use (see the
`forAll nonNull`) and verify the property independently. This runs a default of
100 different sets of input, many of which are complicated messes of
whitespace, backslashes and other special characters. If any of these break the
test, the suite tells me which one, so I can find out what is the problem with
a set of input and whether the contract I employed is incorrect, or the
function tested does not follow the contract.

The current actual fuzzy finder uses six different property contracts that it
needs to satisfy, each four to six lines in size. Testing all of these manually
would lead to a gigantic test suite, and modifying the parameters of the input
would be a huge hassle. With fuzz testing, I can just tweak the `nonNull`
generator to use different rules to generate input, like limiting it to a
certain character range or string length.

The bottom line here is, if your unittests are handwritten, you most likely
cannot be sure your code is behaving in production. There is almost no way you
can think of all the possible ways your code is going to be used, and even if
you could, the amount of testing code would not be feasible.

