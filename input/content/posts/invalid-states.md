title = "Invalid States"
timestamp = "2024-01-13"
tags = ["rust"]
---
I'm currently working on a package manager, and some old wisdom came back to me: making invalid program states impossible to represent.

It turns out that a lot of programs is actually comprised of small finite state machines, more than we would assume at first, and I want to look at representing different lifecycle phases of the same object in particular here.[^1] For example, consider a package manager where a user can request a version of a package. We might model a packages as

```rust
struct Package {
    name: String,
    version: String,
}
```

Now we need to resolve the version the user requests at some point to a concrete version. We might just do that directly on the package, as

```rust
let mut pkg = Package { name, version };
pkg.resolve_version();
```

Now it's probably somewhat bad if we forget to resolve the version on some code path, because then code that assumes that the version is resolved to a known existing one might find its assumptions not holding.

To combat this, we could add a flag to `Package`:

```rust
struct Package {
    name: String,
    version: String,
    resolved: bool,
}
```

But now each consumer needs to check `pkg.resolved` every time, which is not great for ergonomics. Worse than that, we might come back to this code six months later and use a `Package`, completely forgetting to make that check, and we're back to hoping that the version is resolved.

A better way to do this is to make it completely clear whether a version is resolved, and let the computer help us enforce the resolution status:

```rust
/// A request for a package, with a requested version.
struct PackageRequest {
    name: String,
    version: String,
}

/// A package with a resolved version.
struct ResolvedPackage {
    name: String,
    version: String,
}

impl PackageRequest {
    /// Resolves the version of this package.
    fn resolve(self) -> ResolvedPackage {
        let resolved_version = resolve_version(self.version);
        ResolvedPackage {
            name: self.name,
            version: resolved_version,
        }
    }
}
```

With this solution, the act of resolving a version on a `PackageRequest` also changes its type to a `ResolvedPackage`. The compiler won't let us use one in place of the other, and ensures that there is no code path where we might end up with an unexpected unresolved version.[^2]


[^1]: I've actually been thinking about this before in the context of Clojure's [spec 2 alpha](https://github.com/clojure/spec-alpha2/wiki/Schema-and-select), which is a form of runtime gradual typing, and in particular proposes the idea of "selecting" a subset of a map that is actually required in the current context. As such it introduced me to the idea of one single object having different representations in different contexts. In Clojure one would add an additional key for the resolved version, which could then be requested, while in Rust we reuse the same key, but it's the same idea of capturing different lifecycle phases of the same object. I think this is also one of the major advantages of strongly typed languages over weakly typed ones.

[^2]: In this particular case one could also define either a trait or `From` implementations that allow select consumers to use either.
