title = "Testing Hexagonal Architecture"
timestamp = "2020-10-11"
tags = ["clojure"]
---
[*Hexagonal Architecture*](https://web.archive.org/web/20180822100852/http://alistair.cockburn.us/Hexagonal+architecture), also known as *Ports and Adapters*, was first conceived by Cockburn in 2005, and popularised by Freeman & Pryce's [*Growing Object-Oriented Software, Guided by Tests*](http://www.growing-object-oriented-software.com/) in 2009. For those unfamiliar, it describes an application architecture entirely comprised of ports, which are interfaces, and adaptors, which are implementations for those interfaces. The adaptors can depend on other ports, but not on other adaptors. A system is then constructed by selecting a full set of adaptors, depending on the requirements, and composing them using [dependency injection](https://en.wikipedia.org/wiki/Dependency_injection).

A port can represent an external resource or service, but also a logical component of the system, like an HTTP server or a queue handler.


# An Example Port & Adaptor

A simple example for a port could be blob storage. I will be using Clojure in this post, but no prior knowledge is required for understanding.[^1] A port in this case is a protocol, which we implement like so:

```clojure
(defprotocol BlobStoragePort
  (store-object [this loc obj]
    "Store `obj` at  `loc`.")
  (retrieve-object [this loc]
    "Retrieve the object at `loc`.
    Returns `nil` if not found."))
```

Now that we have a port with an interface in the form of abstract method declarations, we can implement an adaptor, for example using S3:

```clojure
(defrecord S3StorageAdaptor [bucket-loc]
  BlobStoragePort
  (store-object [this loc obj]
    (s3/put-object :bucket-loc bucket-loc
                   :key loc
                   :file obj))
  (retrieve-object [this loc]
    (s3/get-object :bucket-loc bucket-loc
                   :key loc)))

(defn new-s3-storage-adaptor [bucket-loc]
  (s3/create-bucket bucket-loc)
  (->S3StorageAdaptor bucket-loc))
```

During tests, we would like to use a blob storage that is much faster and not dependent on external state, so we can use a simple map in an atom:[^2]

```clojure
(defrecord MemoryBlobStorageAdaptor [storage-map]
  BlobStoragePort
  (store-object [this loc obj]
    (swap! storage-map assoc loc obj))
  (retrieve-object [this loc]
    (:loc @storage-map)))

(defn new-memory-blob-storage-adaptor []
  (->MemoryBlobStorageAdaptor (atom {})))
```


# Testing the Port

It has been long known that a direct mapping of tests to internal methods is an anti-pattern to be avoided.[^3] As such we will prefer testing on a port-level over testing on an adaptor-level. In practice that means we assert a certain set of behaviours about every adaptor for a given port by using only the public port methods in our tests, and using the same tests for all adaptors.

```clojure
;; Abstract port test suite

(defn- store-and-retrieve-test [adaptor]
  (testing "store and retrieve returns the object"
    (let [loc "store-and-retrieve"
          obj "test-object"]
      (store-object adaptor loc obj)
      (is (= obj
             (retrieve-object adaptor loc))))))

(defn- not-found-test [adaptor]
  (testing "returns nil for nonexistent objects"
    (is (nil? (retrieve-object adaptor "not-found")))))

;; Specific adaptor tests

(deftest blob-storage-adaptor-test
  (let [adaptors [(new-memory-blob-storage-adaptor)
                  (new-s3-blob-storage-adaptor "test")]]
    (for [adaptor adaptors]
      (store-and-retrieve-test adaptor)
      (not-found-test adaptor))))
```

This has the advantage of establishing a consistent set of behaviours across all adaptors and keeping them in sync. One might wonder about intended behavioural differences between adaptors for the same port, but I would argue that from the outside, all adaptors for a given port should exhibit the same behaviour.[^4] Because we are only using the public interface for testing, any internal differences are conveniently hidden from us.


# The Rest of the System

Now that we have established a port, as well as some adaptors, we can build on top of them. Blob storage is a lower level ports in our system, and we are going to add a higher level port that implements some kind of business logic which requires blob storage.

```clojure
;; Port definition omitted for brevity.

(defrecord BusinessLogicAdaptor [blob-storage-adaptor]
  BusinessLogicPort
  (retrieve-double [this loc]
    (* 2 (retrieve-object blob-storage-adaptor loc))))
```

We are free to use different blob storage adaptors for different systems, for example production, staging, CI, or local development. The business logic adaptor is oblivious to the actual blob storage implementation injected.


# On Mocks & Stubs

The careful reader might have noticed that the dependency injection of different adaptors looks a lot like mocking, and this is very much true. While mocking has been considered more and more problematic in recent years, the fact that we assert the same set of behaviours for our mocks as we assert for the "real components" leads us to much more fully featured and realistic mocks, compared to the ones which are written for specific tests and then rarely touched after.

If the difference in behaviour between different adaptors leads to problems which are not caught by the test suite, the problems is not mocking, but an incomplete behaviour specification for the adaptor in question.


[^1]: This also allows me to gloss over types (or specs) which I would normally add in various places. As an aside, Clojure in particular is not great at this, as record methods cannot be defined by specs, requiring function wrappers.

Also, the example ports & adaptors in this post are modelled after Stuart Sierra's [component](https://github.com/stuartsierra/component) library.

[^2]: For those not familiar with Clojure, an [atom](https://clojure.org/reference/atoms) is reference type that allows us to essentially implement a shared, safely mutable value among Clojure's normally immutable values. Think of it as a pointer with automatic locking.

[^3]: Again, *Growing Object-Oriented Software* has a sub-chapter devoted to this, *Unit-Test Behavior, Not Methods*. It highlights the difference in ease of understanding, but another factor is ease of refactoring, which is significantly higher if the internal method hierarchy is not married to the test suite.

[^4]: If you really need different behaviour in some situations, I would recommend adding a flag or switch controlling this behaviour across all adaptors.
