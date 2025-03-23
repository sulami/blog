title = "Navigating the Rust TLS Landscape"
slug = "rust-tls"
created_at = "2025-03-23"
tags = ["rust"]
---

Both the most popular Rust HTTP client [reqwest] and the probably most popular
web framework [axum] are built on top of [hyper], a low-level HTTP library.
hyper itself does not support TLS, which is needed to make HTTPS requests or
serve them, but rather provides an interface that allows users to bring their
own TLS implementation that fits their requirements. As a consequence, reqwest
has [a whole collection of compile-time feature flags][reqwest features] to
allow users to select a TLS implementation, but is somewhat light on how to make
that choice. This article is aiming to provide a high-level overview.

## Native TLS

[rust-native-tls], as the name implies, uses a TLS implementation native to the
target platform. That is [schannel] on Windows, [Secure
Transport][rust-security-framework] on macOS, and [OpenSSL][rust-openssl] on
Linux.

This has the upsides of using well-tested implementations that are known to be
widely compatible, the resulting binary being smaller thanks to being
dynamically linked against system libraries, and the ability to update those
system libraries independently without having to rebuild the Rust program.

The downside is the reliance on system libraries, both in header-form at
compilation time, and as shared libraries at run time. In most cases this is not
an issue, though certain embedded platforms for example might not have them. It
is worth calling out Debian slim for lacking them as well, requiring
installation of the `libssl-dev` and `pkg-config` packages for compilation, and
`libssl3` for running the program.

There is also the option to "vendor" a version of OpenSSL using the `vendored`
feature, which for Linux targets statically links against OpenSSL, removing the
requirement for a shared library at run time, but also negating most of the
upsides.

## Rustls

The alternative is [rustls], a Rust implementation of TLS. Rustls is generally
more portable because it has no external dependencies in the form of shared
libraries. Supposedly it is also [faster than OpenSSL][rustls-fast].

Its main downside is that Rustls only supports TLS 1.2+, and explicitly does not
support [a variety of older features][rustls-no], which limits compatibility
with older systems.

As an aside, Rustls needs a provider for cryptographic primitives, and by
default uses AWS' [aws-lc-rs], but also includes [ring] as an alternative
provider. There are also third-party [rustls-providers], and the option to bring
your own provider. For most use cases the default provider is probably fine, and
it seems like the main reason to choose an alternative provider would be a
requirement to support a platform [aws-lc-rs does not support][aws platforms].

### Root Certificates

While [rust-native-tls] generally uses root certificates native to the platform,
such as the macOS keychain, [rustls] allows specifying where to source root
certificates from. Similarly to the choice of libraries, there is a trade-off
between using platform-native certificates and including certificates with the
program.

One option is to include certificates in the program, by using either [webpki's
certificates][webpki-roots], which is a set of root certificates by Mozilla,[^1]
or by supplying your own certificates manually, either at compile-time or at
run-time. In the latter case, a user could inject their own root certificates if
needed, like a self-signed certificate for a corporate proxy. If root
certificates are baked into the program at compile-time, supporting custom
certificates requires recompiling.

Another option is to use the certificates already installed on the system, just
like [rust-native-tls] does. This of course means certificates need to be
present where the program is run. Again, Debian slim does not include
certificates by default and requires installing `ca-certificates`. For this
option, the preferred solution is to use [rustls-platform-verifier] which has
better support for distrusting decisions but is not widely supported by other
crates yet,[^2] though there is also the older but more widely supported
[rustls-native-certs].

A third option is to use a combination of both, which can be done with
[rustls-platform-verifier], as it can use the platform's native certificate
validation mechanism and also fall back to [webpki-roots] and/or
[rustls-native-certs].

[^1]: This set has actually been the default set in many Linux distributions for
    a long time.
[^2]: At the time of writing at least, see e.g. [this
    issue](https://github.com/seanmonstar/reqwest/issues/2159).

## Recommendation

For software deployed to users [rust-native-tls] is still the safest choice, as
it will work in a wider range of circumstances, though [rustls] can work, too,
especially with native root certificates. For software deployed in a controlled
environment, [rustls] is likely less hassle and more secure.

[reqwest]: https://github.com/seanmonstar/reqwest
[axum]: https://github.com/tokio-rs/axum
[reqwest features]: https://docs.rs/reqwest/latest/reqwest/#optional-features
[hyper]: https://github.com/hyperium/hyper
[rustls]: https://github.com/rustls/rustls
[rust-native-tls]: https://github.com/sfackler/rust-native-tls
[rustls-native-certs]: https://github.com/rustls/rustls-native-certs
[rustls-platform-verifier]: https://github.com/rustls/rustls-platform-verifier
[aws-lc-rs]: https://github.com/aws/aws-lc-rs
[ring]: https://github.com/briansmith/ring
[rustls-providers]: https://github.com/rustls/rustls?tab=readme-ov-file#third-party-providers
[aws platforms]: https://aws.github.io/aws-lc-rs/faq.html#can-i-run-aws-lc-rs-on-x-platform-or-architecture
[rustls bench]: https://github.com/aochagavia/rustls-bench-results?tab=readme-ov-file#3-conclusions
[rustls-native-certs]: https://github.com/rustls/rustls-native-certs
[schannel]: https://github.com/steffengy/schannel-rs
[rust-openssl]: https://github.com/sfackler/rust-openssl 
[rust-security-framework]: https://github.com/kornelski/rust-security-framework
[rustls-no]: https://docs.rs/rustls/latest/rustls/manual/_04_features/index.html#non-features
[rustls-fast]: https://www.memorysafety.org/blog/rustls-performance-outperforms/
[webpki-roots]: https://github.com/rustls/webpki-roots
