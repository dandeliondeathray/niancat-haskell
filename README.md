# niancat-haskell

Because why not!

## Current status

This is just a skeleton so far, so there's lots of work to do before we
have feature-parity with the Scala version.

- [ ] Setting puzzles
- [ ] Solving puzzles
- [ ] Submitting unsolutions
- [ ] Showing stats
- [ ] Persistent state, possibly backwards-compatible with niancat-scala

## Development

1. Open the `niancat-haskell` in a VS Code Remote Development environment.  
   It will download a pretty large Docker image, so go grab a coffee and a good book
   and check back every now and then.

1. Once it's loaded, open a Haskell file and hover something to initialize the Haskell extension.  
   This starts another round of downloading the world. You didn't finish your book already, did you?

1. Once tooling is set up, you'll have use for these commands:

   - `stack build --file-watch --test` - build project and run all tests,
     and do it again if anything changes.

   - `stack build --exec niancat-exe` - run a server on port 3000

## Building for production

1. `docker build . -t niancat-haskell` builds an image. This
   also downloads the world; however, unless you make package changes,
   the long-running steps will be cached.

1. `docker run -p 8080:3000 niancat-haskell` runs it and exposes the API
   on port 8080.
