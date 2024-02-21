title = "Concurrent I/O in Haskell"
slug = "concurrent-io-in-haskell"
timestamp = "2015-09-11"
tags = ["haskell"]
---
Today just after waking up, I have been tweaking the last bits and bops of [TinfoilSub](https://github.com/sulami/tinfoilsub), a satiric fun/learning project of mine that scrapes YouTube channels to replicate a subscriptions page without requiring a Google account. The core of this little program is the `runServer` function that uses [Scotty](https://hackage.haskell.org/package/scotty) to host a local page that displays the results, while the list of channels is read from a file. In the beginning, this function looked something like this:

```haskell
runServer :: [String] -> IO ()
runServer channels = scotty 3000 $ do
  get "/" $ do
    videos <- fmap (sort . concatMaybe) . liftIO . sequence $
                map scrapeChannel channels
    html . renderVideos $ take 50 videos
  -- ... and more routes
```

Now, imagine you have more than just a couple of channels in there. This works fine for 2-3 channels, but each channel adds about a second of runtime each time the page is refreshed and the channels are re-scraped, which quickly amounts to way to much. Naturally, I went on to profiling and trying to figure out how to improve the scaling capabilities, and one of the first things to come across is using several threads (duh). Pretty basic stuff, so lets add and import `Control.Parallel`, instruct the RTS to use a number of threads that suits the CPU in use and everything should be fine. Right?

This is the new code:

```haskell
runServer :: [String] -> IO ()
runServer channels = scotty 3000 $ do
  get "/" $ do
    videos <- fmap (sort . concatMaybe) . liftIO . sequence $
                parMap rpar scrapeChannel channels
    html . renderVideos $ take 50 videos
  -- ... and more routes
```

This looks neat and one might think it does what we want it to do. But looking at the actual performance when using 15 channels, this code is only about 20% faster, even when using 8 or more threads on a CPU with more than enough cores. Time for some more in depth analysis. The first thing I decided to look at was good old top. YouTube is not too slow in terms of respond times, so with a couple of threads one could assume we get to start scraping the page fairly early on, which results in increased CPU usage. But looking at top while refreshing the page revealed that the CPU was idling for a good 10 seconds before starting to scrape at all. Scraping was then run concurrently for the collected pages, so all cores were used and the actual scraping only took a second.

A look at Wireshark confirmed the problem. Wireshark recorded a new request to YouTube every second before scraping started. I looked around a bit in [Scalpel](https://hackage.haskell.org/package/scalpel), the scraping library I used and [the curl bindings](https://hackage.haskell.org/package/curl) that were used for the actual requests, but nothing indicated any problems with making multiple requests at the same time.

So, now for the big reveal. For those who do not know, Haskell's `Parallel` only works for pure parts of code, so no I/O, which hinders you from creating things like race conditions, deadlocks and other really bad stuff. That is a good thing. So, if you want to do concurrent (or parallel) I/O, you need to use `Concurrent`, which brings internal threads, thread communication tools and this kind of stuff. Now, this is all pretty dangerous territory in that with these tools, we can create the bad things mentioned above, which we do not want to risk, at least not if there are better alternatives. And as it turns out, there are. Use [async](https://hackage.haskell.org/package/async). Async is essentially a wrapper around all the evil stuff that makes it harder to shoot yourself in the foot. For us, there is this neat function called `mapConcurrently`, which does exactly what we want. Do a `map`, do it concurrently, and do it with `IO`. So this is what we get:

```haskell
runServer :: [String] -> IO ()
runServer channels = scotty 3000 $ do
  get "/" $ do
    videos <- fmap (sort . concatMaybe) . liftIO $
                mapConcurrently scrapeChannel channels
    html . renderVideos $ take 50 videos
  -- ... and more routes
```

And it does exactly what we want, it starts scraping as soon as the first request has returned a page for use, which reduces the runtime by more than half.
