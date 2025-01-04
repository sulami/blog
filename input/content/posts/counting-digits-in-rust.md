title = "Counting Digits in Rust"
timestamp = "2025-01-04"
tags = ["rust"]
---

The just concluded edition of [Advent of Code][aoc] included a problem that
involved the number of digits of an integer. The naive solution many folks reach
for is printing the number to a string and checking the length of the string:

```rust
fn digits(n: u64) -> u32 {
    n.to_string().len() as u32
}
```

My solution used a logarithm with base ten instead, which I deemed more elegant:

```rust
fn digits(n: u64) -> u32 {
    n.checked_ilog10().unwrap_or(0) + 1
}
```

But then, while exchanging solutions online, someone[^1] shared their supposedly
optimized solution with me:

```rust
fn digits(n: u64) -> u32 {
    match n {
        0..=9 => 1,
        10..=99 => 2,
        100..=999 => 3,
        1_000..=9_999 => 4,
        10_000..=99_999 => 5,
        100_000..=999_999 => 6,
        1_000_000..=9_999_999 => 7,
        10_000_000..=99_999_999 => 8,
        100_000_000..=999_999_999 => 9,
        1_000_000_000..=9_999_999_999 => 10,
        10_000_000_000..=99_999_999_999 => 11,
        100_000_000_000..=999_999_999_999 => 12,
        1_000_000_000_000..=9_999_999_999_999 => 13,
        10_000_000_000_000..=99_999_999_999_999 => 14,
        100_000_000_000_000..=999_999_999_999_999 => 15,
        1_000_000_000_000_000..=9_999_999_999_999_999 => 16,
        10_000_000_000_000_000..=99_999_999_999_999_999 => 17,
        100_000_000_000_000_000..=999_999_999_999_999_999 => 18,
        1_000_000_000_000_000_000..=9_999_999_999_999_999_999 => 19,
        10_000_000_000_000_000_000..=u64::MAX => 20,
    }
}
```

I was curious if this "hand-rolled" solution was actually faster than either of
the other ones, so I put together a quick benchmark, both for inputs of zero and
random, non-zero 64-bit integers:

```
digits/string/zero      time:   [23.449 ns]
digits/string/non-zero  time:   [32.263 ns]
digits/log/zero         time:   [313.07 ps]
digits/log/non-zero     time:   [1.5606 ns]
digits/match/zero       time:   [353.12 ps]
digits/match/non-zero   time:   [5.6750 ns]
```

It turns out the `match` solution is four times slower than the logarithm
solution, though the `string` solution is another six times slower. We can see
that both `match` and logarithm perform much better if the input is zero, the
former because it matches early, and the latter because it checks for zero as
part of the logarithm. The `string` solution does not get a similar speedup,
because it has to allocate a string either way.

[If we look at the assembly for the latter two][asm], we can see that `match`
generates a chain of comparisons for the `match` arms, as the first matching arm
defines the result.[^2] The compiler translates this into the assembly
equivalent of  a chain of `if` statements with comparisons. The logarithm
solution on the other hand compiles into about 30 instructions that do not make
a lot of sense at first glance, mostly bitwise operations but also some
additions and multiplications, which apparently add up to a logarithm with base
ten.[^3] Because `checked_ilog10` returns `None` for zero, there is also the
aforementioned check for zero at the very start returning zero and skipping the
rest in that case.

One might reason that the `match` solution is more dependent on the number of
digits, as the number of comparisons required scales directly off that, while
the logarithm takes more or less constant time. And indeed, for three digits the
`match` solution is twice as fast as the logarithm:

```
digits/match/three      time:   [629.78 ps]
digits/log/three        time:   [1.5698 ns]
```

As can be extrapolated from this result, the crossover point is at six digits,
so it seems that the optimal apporach depends on how many digits you expect it
to have. Personally, I will stick with the logarithm for the obvious reasons. Of
course, if performance matters to you, do measure yourself with your own
compiler and on your own hardware.

[^1]: Not sure if they're okay being named, so I'm defaulting to keeping them
    anonymous.
[^2]: Which is also why the order of arms matters if several arms can match an
    input.
[^3]: Further research reveals that the specific logarithm implementation
    actually varies a lot with the processor targeted.

[aoc]: https://adventofcode.com/2024/day/11
[asm]: https://godbolt.org/#z:OYLghAFBqd5TKALEBjA9gEwKYFFMCWALugE4A0BIEAZgQDbYB2AhgLbYgDkAjF%2BTXRMiAZVQtGIHgBYBQogFUAztgAKAD24AGfgCsp5eiyahSAVyVFyKxqiIEh1ZpgDC6embZMDzgDIEmbAA5TwAjbFIDAAd0JWIHJjcPL2jY%2BKF/QJC2cMiea2xbeyERIhZSIiTPb3ybbDsE0vKiTOCwiINLZqqU2rKK1uzcqQBKa3QzUlROLgBSACYAZlmAVgAhJnQAfTZjYEZVgBFZrQBBGiYAakJgYiUIb0uzADZpEcuAWlnF3CfF%2BcuswA7GsTqdLhDLkwAHSoJD1ADW2EwWwY6GAPC0EBG0LMTAA7qQWFEtmQIFp3gs1pceGDgcczlwxvRuCt%2BN4uDpyOhuAAlCxES5KCZTbCApZ8chEbRMsbwlg4SLYwzcaT8NggFZacgcrk8rj8JQgbXSzlM8hwWAoDA4fDEMiUah0RisDjcSWCYRiCScGRyYTKNSaM3kfTzQx7EDmSwFIoJJxMVzuapScN%2BAJtHIdfIxOLFRLJ3rh3PpJiDdp5WP1fNNCo9GrhuoNEr9FoZobZ6yt%2BuprvNctZvJjYWTabcBbLdabHZ7A4rBnnK43O4PEBPV7vL4/P4A4Ggs6Qy67IhwqGAkFgw%2BHrTQ6HfQ4ATkBi2O2/yl6vEMxt/vD6f9%2B%2BX5ww/T9MRvO8Xz/f8X0Ay5FnIECrx4LYtFQn9IK2KDn1fX5ZEQw9MRQtCIMfB9ML/bDYJWBCD0/GlUKI8Dfz/cjoJwy5nho8E6OQ1CtEY9DH1Y1jKO3IEuLo%2BjGIEkiWKgkSAO3AAOCSeIYviZOYsj5KwxTfgfVTQOkjS%2BME7S5IstjYMxQykP4kz1OI5jhJ0ii9Jpd9aNAxz7J8szLJctyYLfYCvKQ4yfM0jDXPM2LRN%2BHh4PwyFCIc3z0v8wLYoU4KErwsKCMitKotI7KYpy9ieGo5Kvwi9K6rMrKmt03KaU4mqpOKrqnMgsqApi%2BKaXEjqwLqsbTNk/qpri9yeBUkbxqKvzZOa6aKusgyRvq7rtp6w4XmkEAQAAWVOAANQb5m1RD6TpIEF3NFkuDZHUZW5PkBSFEVpnFf5%2BFNHQRjlbAFQ6ZUERAaQH2hJSlMWZ4Vn%2BJTniBB9pAR2QnrVcgNS1V6Q31Q1jSlGUgfICGoZhuGEaRlG0YxlUuEWdk3sJkmzTGS0EHgCBrXQNgogYCJHQgDABaFyJSDmxZtWdIgIiNCBQje0IAnKABPd1%2BFV1hSHVgB5UJdGrLXyDFjhhH1ph6E1kMcFCMxgBcCR6CNXh%2BBwXYTEkO2CFIasCAAN2wN2uWwdR6jMeXTYCeWnq5egCFCIk9bcHA3qIUgCA1d3yGD0hQlibBDmwL39gCUAOYEIxgCUAA1AhsHxfWomYU3PVEcRJD9DvAw0N79HyIwTCjAVDCTo1IDGdAonzN2PhcS4Z6ID5GGD%2Bh70WT5F6iYwCFQe8WELip%2BHQfOs5wSflSbfMEyTZIanIdMsgrVI8wSHsczSfMB2GWpCgDi2bohZH430aK2X%2BnYuh1hAZ0CB7ZX48GHN9X0zJWQswJtwS40YTw0hhosaEWhLgQDtCQUgv0kH/VJsDUGSo0FcGxrjbUupT7cCJiaUmFpEC8zQPzQWjAKBUFFnwiWUZpaywYPLUgitlYhh1hrU28i9aG2NnYU25tmBECtjbN69tHbO3oK7U2ntZwzC5IQf2DRg6h34OHSO0dc6x0KG9ROycNZpzMf9LOOdJT50LioEuZdE4jyrjQGu9dG7N1bhyD08hvTd1kL3FQ/cQxhgjCPHB49QhX2nrPBI89F7L1Xtgdem9t6XF3kwfeh9j5EFPufAgl94DDgAc2bwEBnCfyfomSBlYSz5i6f0hIvTOitJrN2WB/84xAIGAgwccDgEPwWbMl%2B8ykHjFHJweY9CXosPelwbBApUB4LhoQ4hpCyC/W2VQjmYwIZamhDwHgzxFjoyBIlK6iNOJY3VJqZhrM2HWGJgDWU5M/mPOea8lGHytBfMZszfGepAUgrJk9eYGCkUGnZoDMY%2Bc4iOGkEAA%3D%3D%3D
