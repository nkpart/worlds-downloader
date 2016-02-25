World Pipe Band Championships 2015 Audio Downloader
---

There are no longer CDs being produced of the World Pipe Band Championships, but the performances are available via the BBC.

This project downloads the streams, and extracts the audio.

Prerequisites
----
 
    brew install rtmpdump
    brew install youtube-dl

And stack (for building haskell)

Usage
----

    stack build && stack exec worlds-downloader out-dir
