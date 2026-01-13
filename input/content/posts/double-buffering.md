title = "Double Buffering for Event Consumers"
created_at = "2026-01-13"
tags = ["rust", "software-design"]
---

Imagine a logging system where we have many threads producing log events, and we
have a single thread that consumes those log events and outputs them somewhere.
A simple implementation for this would use a `Mutex<VecDeQueue>`,[^1] or
perhaps a ring buffer if you are feeling fancy, so that producers can append
events at one end and the consumer can pop them off the other, preserving the
order.

In many cases we might want to batch consumption to reduce static overhead, so
instead of popping items off the buffer one by one, we want to take a lot of
them at once. Now that we are taking, and potentially processing, many items, we
could be holding the lock for much longer than before, blocking producers.
Logging latency would spike every time the consumer consumes another batch of
events.

We could just
[`drain`](https://doc.rust-lang.org/std/collections/struct.VecDeque.html#method.drain)
the buffer into a new `Vec` and release the lock to buy ourselves time for the
processing without holding the lock, but we would still perform an iteration
over potentially many items while holding the lock.

A neat pattern we can adapt from graphics programming in this case is double
buffering, a design pattern where one has two (or even more) buffers to separate
writing and reading. It is mainly used to allow incrementally building up a
complete buffer without reading inconsistent data, but we can also leverage it
here to ensure consistently low latencies for producers.

Instead of a single buffer we have two buffers, call them `A` and `B`, and we
will just use a `Mutex<Vec>` in this case as we just need to append events. We
start by using the `A` buffer, appending events from the producers, just as
before. At some point the consumer runs and acquires the lock. While holding the
lock, all it does is [swap out](https://doc.rust-lang.org/std/mem/fn.swap.html)
the buffer with the empty `B` buffer, which is constant time.[^2] After the
consumer releases the lock, the producers now append items to the `B` buffer,
and the consumer can take its time to process the events in the `A` buffer.
After it is done, it clears the `A` buffer, and for the next batch swaps the
buffers back, now receiving a filled `B` buffer.

If we have memory to spare, we can leave both buffers allocated as well,[^3] so
that after the first two batches we would not expect any further allocations, as
long as event volume stays constant. This further ensures reliably low latencies
for event producers, as growing a `Vec` can be expensive.

This pattern can be applied to all kinds of event-based fan-in systems where
consumption is batched and producers are latency-sensitive. In fact, I have used
this pattern for `tracing` data in
[tracing-datadog](https://github.com/sulami/tracing-datadog/blob/20497d86d9512c1fc4cbbd352828bd774ebfede7/src/export.rs#L80).

[^1]: Generally this would be wrapped in an `Arc` as well, but I'm leaving
    those out here for readability.
[^2]: All `swap` moves is the `Vec` struct itself, which is just a pointer and a
    size field, so it moves just a few bytes regardless of the size of the
    `Vec`.
[^3]: Maybe shrink them to a reasonable size so that temporary volume spikes
    don't consume memory for the rest of the program's lifetime.
