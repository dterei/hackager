# Hacking on Hackager

Source files:

 * `Hackager.hs`      -- Main entry point, parses command line arguments.
 * `Record.hs`        -- Entry point for record (build) mode.
 * `HackageMonad.hs`  -- Monad and support for build mode. Monad is `Hkg`, a
                         state monad with state `HkgState`.
 * `RecordOptions.hs` -- Parses command line flags for record mode.
 * `BuildManager.hs`  -- Orchestrates building all packages.
 * `Build.hs`         -- Handles building a single package.
 * `BuildTools.hs`    -- Wrappers to run various external build tools.

 * `Report.hs`        -- Entry point for report mode.

 * `Parallel.hs`      -- Handles parallelizing Hackager.
 * `Utils.hs`         -- Misc functions.

Building Process:

 1) Parse command line flags
 2) Get list of packages to build, either from command line or all of hackage.
 3) Create the needed build directory structure.
 4) Run `cabal install --dry-run` on each package first to see if installable.
 5) Run `cabal install` for each installable package. Do so in a new and
    temporary package configuration.
 6) Dump all results.

