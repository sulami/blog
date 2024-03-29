title = "Genetic Programming in Clojure"
timestamp = "2018-11-17"
tags = ["clojure"]
---
# The Theory

Like most programmers I have always had a vague interest in AI, and one of its branches that requires less complicated maths than recurrent neural networks which are the most well known one, is genetic programming. The idea of genetic programming is quite simple[^1]:

1.  You build something that is parameterised in key places
2.  You build a scoring function to assess the performance of your something with a set of parameters
3.  You randomly adjust (mutate) your parameters in some way a couple of times and compare the score of each set
4.  You take the best one or ones as a base to start a new round of mutations and scoring
5.  Basically just repeat steps 3 & 4 for a while and your parameters will tend towards a maximum score

Depending on a variety of meta-parameters that control for example the size of each generation or the nature of the mutations you might just find a local maximum, but often times this can yield pretty good results for a variety of problems.


# The Practice

I have toyed around with this over the last couple of days and built an very simple abstract implementation in Clojure, which I am going to share here (and eventually will be somewhere on Github as well). Let us explore it from the inside out.

First of all we need to be able to generate some mutations of our specimen. Because we do not assume anything about the specimen, this ends up being quite simple because a lot of the heavy lifting is done outside of this implementation as it is specific to the problem in question.

This returns a potentially infinite list containing first the specimen passed in, and then as many mutations of it as we want.

```clojure
(defn mutate
  "Generator that mutates a base, the first element being the base."
  [base mutator]
  (concat [base]
          (repeatedly #(mutator base))))
```

Next we also need to be able to score it. In this case we would like to attach the scores to the specimens so that we can use them to sort and select specimens without losing the specimens themselves.

```clojure
(defn attach-score
  "Attaches the score to a specimen."
  [score-fn specimen]
  [specimen (score-fn specimen)])
```

Now let us begin to tie these together. A single generation should take a base specimen, mutate it a couple of times, score each of them, and then select the "fittest" based on the scores[^2]. Note that in this implementation a lower score is better. To change this just reverse the sorting.

```clojure
(defn generation
  "Picks out the best one from a generation."
  [base mutator score-fn gen-size]
  (->> (mutate base mutator)
       (take gen-size)
       (map (partial attach-score score-fn))
       (sort-by second)
       first
       first))
```

And to finish off, we just need to run a number of generations, each based on the previous one's winner.

```clojure
(defn evolution
  "Generator for generations."
  [base mutator score-fn gen-size]
  (iterate #(generation % mutator score-fn gen-size) base))
```

The lazy nature of this implementation is allows us to inspect intermediate results easily, as we can see the path evolution has taken in the form of each generation's winner.


# The Actual Practice

Now this above is actually not that much code, and it is very abstract in its nature, so let us have a look at what it looks like when we actually use it. A simple example would be approximating a single number that is hard to approximate, like √2.

Our specimen is just a float, and any will do as the initial seed. It is itself the only parameter.

```clojure
(def base 0.0)
```

To mutate it, we just adjust it by a random amount within `0.5` in either direction.

```clojure
(defn mutator [base]
  (-> (rand)
      (- 0.5)
      (+ base)))
```

Our scoring function is cheating a little, because we already know the target, we can just compare against it and use the distance as the score.

```clojure
(defn score-fn [x]
  (-> x
      (- (Math/sqrt 2))
      Math/abs))
```

Now when we run this, we can see how it approximates the target value over time (`√2 ≈ 1.4142`).

```clojure
(take 6 (evolution base mutator score-fn 25))
;; => (0.0
;;     0.33079046010191426
;;     0.7509224756253191
;;     1.2164225056336746
;;     1.3768753691848903
;;     1.4125030676422798)
```

Because `evolution` returns an infinite sequence, we can just use `nth` on it to get the winner after a certain number of generations.

While this is a very simple example, I am currently working on a way of using this to build and mutate a Clojure [S-expression](https://en.wikipedia.org/wiki/S-expression) and score it by running a series of unit tests against the generated code. If this works out I might write about it here soon.


[^1]: If you are into more visual examples, this I believe is a very good practical example: <http://rednuht.org/genetic_cars_2/>

[^2]: This also only keeps the best specimen in every generation, which makes the code much simpler. For actual real world usage it might be beneficial to keep the best `n` specimens in every generation to avoid running into local maxima. This would make the mutation slightly more complex though because there would be several base specimens which need to be mutated, so I decided to leave out this feature for the purposes of explanation.
