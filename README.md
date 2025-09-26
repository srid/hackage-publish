# hackage-publish

**Goal**: A simple script to publish Haskell packages to Hackage with little fanfare.

## What it does

- Creates source distribution with `cabal sdist`
- Retrieves Hackage credentials from 1Password
- Uploads sdist *and* haddock to Hackage with `cabal upload --publish`
