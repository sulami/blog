title = "The Shape of Tests"
slug = "test-shapes"
timestamp = "2021-03-03"
tags = ["clojure", "software-design"]
---
Many tests for an operation iterate over a mapping of different inputs to expected outcomes. By looking at the tests for a single operation as the same test with different inputs and output expectations, we can start to question how we should model those tests.


# Tests as Matrices

By simply enumerating every possible combination of input values, we can construct a matrix with as many dimensions as inputs. We can then define the expected result for each set of inputs, and write a generalised test function:

> ∀ input∈{(a, b, &#x2026;, n) | a∈A, b∈B, &#x2026;, n∈N} f(input)

The number of possible test cases is thus:

> &vert; inputs | = |A × B × ⋯ × N|

As soon as our operation accepts an input that has more than a few possible values, that is any kind of number,[^1] string, or complex data structure, enumerating every possible input combination becomes impractical. Instead we can resort to groupings of values via properties.

This is a test matrix for division which uses properties instead of values, with the rows being dividends, and the columns divisors:

| ÷        | Positive | Zero      | Negative |
|-------- |-------- |--------- |-------- |
| Positive | Positive | undefined | Negative |
| Zero     | Zero     | undefined | Zero     |
| Negative | Negative | undefined | Positive |

Matrices like this are necessarily exhaustive,[^2] and force us to think about the result for every possible combination of the input values we have included.

This is an implementation of the same property matrix in Clojure:

```clojure
(ns division-matrix-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

(defn safe-divide
  "2-arity `/`, but returns `nil` on division by zero."
  [dividend divisor]
  (try
    (/ dividend divisor)
    (catch ArithmeticException _
      nil)))

(defmacro test-matrix
  "Generates tests for a two-dimensional test matrix."
  [test-fn matrix]
  (let [columns (rest (first matrix))
        rows (map first (rest matrix))
        combinations (for [[row idy] (map #(vector %1 %2) rows (range))
                           [col idx] (map #(vector %1 %2) columns (range))]
                       [row col (-> matrix
                                    (nth (inc idy))
                                    (nth (inc idx)))])]
    `(doseq [combination# [~@combinations]]
       (apply ~test-fn combination#))))

(deftest safe-division-test
  (let [gen-input
        (fn [kind]
          (case kind
            :pos (gen/generate (s/gen pos-int?))
            :neg (gen/generate (s/gen neg-int?))
            :zero 0))]

    (test-matrix

     (fn [x y result-pred]
       (let [dividend (gen-input x)
             divisor (gen-input y)]
         (is (result-pred (safe-divide dividend divisor))
             (format "Failed with: %s / %s" dividend divisor))))

     [[nil    :pos   :zero  :neg ]
      [:pos   pos?   nil?   neg? ]
      [:zero  zero?  nil?   zero?]
      [:neg   neg?   nil?   pos? ]])))
```

In this case we are testing a safe variant of the division function `/`, which returns `nil` if the divisor is zero. This simplifies the testing process, because we do not have to include any exception catching logic in our test function, or invent a notation to mean *this set of inputs should result in a thrown exception*.

It is worth noting that such a direct interpretation of a matrix is only possible in a language as malleable as Clojure. In other languages, we might have to resort to enumerating a set of `(dividend divisor result)` tuples, losing the guarantee of covering all possible combinations.

But even in Clojure, more than two dimensions in this matrix will quickly become unwieldy and hard to follow, and a tuple-based approach would scale better to larger numbers of input parameters.


# Tests as Trees

Another way we could structure our tests is as a tree. A tree does not have to be exhaustive the same way a matrix has to be.[^3] We can omit certain combinations of inputs by pruning their branches. In this way we are implying that if a single input has a given value, it defines the result regardless of the other inputs' values.

In the division example all branches with a divisor of zero could be collapsed into a single case, as the dividend does not matter in this case. This only works if the first level of branching describes the divisor, and the dividends are on the second level.

```clojure
(ns division-tree-test
  (:require [clojure.test :refer [are deftest testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

(deftest safe-division-test

  (testing "with a positive divisor"
    (let [divisor (gen/generate (s/gen pos-int?))]

      (testing "and a positive dividend"
        (let [dividend (gen/generate (s/gen pos-int?))]
          (is (pos? (safe-divide dividend divisor)))))

      (testing "and a zero dividend"
        (let [dividend 0]
          (is (zero? (safe-divide dividend divisor)))))

      (testing "and a negative dividend"
        (let [dividend (gen/generate (s/gen neg-int?))]
          (is (neg? (safe-divide dividend divisor)))))))

  (testing "with a divisor of zero"
    (let [dividend (gen/generate (s/gen int?))]
      (is (nil? (safe-divide dividend 0)))))

  (testing "with a negative divisor"
    (let [divisor (gen/generate (s/gen neg-int?))]

      (testing "and a positive dividend"
        (let [dividend (gen/generate (s/gen pos-int?))]
          (is (neg? (safe-divide dividend divisor)))))

      (testing "and a zero dividend"
        (let [dividend 0]
          (is (zero? (safe-divide dividend divisor)))))

      (testing "and a negative dividend"
        (let [dividend (gen/generate (s/gen neg-int?))]
          (is (pos? (safe-divide dividend divisor))))))))
```

This might look more verbose, but in exchange we get a unique label for every tree branch,[^4] which can improve readability. The nesting also naturally lends itself to lexical scoping, so we only have the values in scope which apply on a given branch.

A key advantage of the tree structure is flexibility. If one of the branches requires special code, we can confine it to that branch, avoiding complicating the remaining branches more than necessary.

Trees also scale better with larger numbers of inputs or options for inputs. A tree might grow overly wide or deep, but we can split it if that becomes a problem.

There is a downside to omitting branches though. If we change our `safe-divide` function to return different results depending on the dividend when the divisor is zero, our tests might still pass, depending on the specific inputs used, but we will lose test coverage for certain code paths. We have chosen to not test certain input combinations, and we need to be aware of this omission when we are changing the code under test.


# Tests as Definitions

Considering the formula describing the generalised test function above, we could also consider translating this directly into code. This can work, but only if we can test results without re-implementing large parts of the code under test, otherwise we are overly coupling the tests to the code. In the division case, we can decide the sign of the result[^5] based on the signs of the inputs.

```clojure
(ns division-spec-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

(defn- check-safe-divide-result [{{:keys [dividend divisor]} :args
                                  ret :ret}]
  (cond
    (zero? divisor) (nil? ret)

    (zero? dividend) (zero? ret)

    (or (and (pos? dividend) (pos? divisor))
        (and (neg? dividend) (neg? divisor)))
    (pos? ret)

    :else (neg? ret)))

(s/fdef safe-divide
  :args (s/cat :dividend number?
               :divisor number?)
  :ret (s/nilable number?)
  :fn check-safe-divide-result)

(deftest safe-divide-spec-test
  (let [check-result (stest/check `safe-divide)]
    (is (not check-result)
        (format "Failed with: %s"
                (-> check-result
                    first
                    stest/abbrev-result
                    :failure
                    ::stest/val)))))
```

This solution is specific to Clojure, though many other languages have property based testing tools that work similarly.

By adding a spec to our function, we can run a large number of different inputs against our function, and assert a property about the result based on the inputs. It will even shrink the inputs to find the simplest set of inputs to trigger a spec failure.

This means we do not have a programmer writing a matrix or a tree by hand anymore, which has some advantages. The main one being that a programmer might not consider all possible inputs.

```clojure
Fail in safe-divide-spec-test
Failed with: {:args {:dividend ##NaN, :divisor 0}, :ret ##NaN}

Fail in safe-divide-spec-test
Failed with: {:args {:dividend 1, :divisor ##Inf}, :ret 0.0}

Fail in safe-divide-spec-test
Failed with: {:args {:dividend 6.812735744013041E-108, :divisor 2.7578261315509936E216}, :ret 0.0}
```


# Conclusion

The optimal shape of a test depends mainly on the structure of the inputs to the operation we are testing, as well as its nature.

For pure functions which we expect to use widely and change rarely, property-based testing can be desirable to avoid unintended consequences. There is also a certain speed requirement for test shrinking to work effectively.

Operations with a small number of possible inputs can also be tested via test matrices, which have fewer limitations, but do not guarantee correctness,[^foo] as only the programmer can assert the completeness of the matrix.[^bar] They are easy to extend with additional values for parameters, but harder to extend with additional values. Their declarative nature can be useful for documentation purposes.

At the other end of the spectrum, tree-shaped tests are the most flexible, and scale best for larger operations with many inputs. If different branches require fundamentally different setup, test trees can isolate that complexity to where it is required. They also require the most care to keep tidy, and have a tendency to sprawl if unsupervised.

[^1]: With more than 8 bits at least. One could reasonably enumerate 256 different inputs for an operation, for correctness proven by test.

[^2]: Unless we designate a special result to mean "*do not test this set of inputs*".

[^3]: Again, assuming we are using a matrix based approach, rather than enumerating combinations manually.

[^4]: We can also make use of useful property of `clojure.test`: if a test fails, it will traverse the `testing` statements upwards and concatenate the names, providing us with the path of the failed test. For example "*with a negative divisor and a zero dividend*".

[^5]: Or the existence, for that matter.

[^foo]: Foo.

[^bar]: Bar!
