# Hackager

Hackager is a tool to compile all of the Haskell Hackage package
repository. This is useful for testing Haskell compilers.

## Using
Hackager consists of two tools. One is ''Hackager'', the tool used to
compiler all of Hackage and the other is ''HackagerReport'', the tool
used to perform a simple comparison of two different runs of Hackager.

''Hackager'' is invoked with the following options:

~~~~ {.sh}
$ Hackager <name> <cabal> <ghc> <ghc-pkg> <dep-flags> <pkg-flags>
           <threads> [pkgs]

    name:      A name by which the results of this Hackager run will
               be referred, e.g. "ghc-6.12.1"
    cabal:     The path to the cabal program to use
    ghc:       The path to the ghc program to use
    ghc-pkg:   The path to the ghc-pkg program to use
    dep-flags: The flags to use when compiling dependencies of a package
               e.g. "" or "-XFoo -XBar"
    pkg-flags: The flags to use when compiling a package
               e.g. "" or "-XFoo -XBar"
    threads:   Number of threads to use to build in parallel
    pkgs:      An optional list of packages to build. If not specified, all
               of hackage is built
~~~~

For example, here is a run with GHC, no special options and using 4
threads (note that this generally takes a long time, i.e. a few days):

~~~ {.sh}
$ Hackager normal /usr/local/bin/cabal \
                  /usr/local/bin/ghc \
                  /usr/local/bin/ghc-pkg \
                  "" "" 4
~~~~

Then another run, this time using ''-XAlternativeLayoutRule'' to
compile each package (but not the build dependencies of the package):

~~~~ {.sh}
$ Hackager alternative /usr/local/bin/cabal \
                       /usr/local/bin/ghc \
                       /usr/local/bin/ghc-pkg \
                       "" \
                       "--ghc-option=-XAlternativeLayoutRule"
                       4
~~~~

And finally a comparison of the results:

~~~~ {.sh}
$ HackagerReport normal alternative

                            normal
                         Buildable Build failed Deps failed Not tried
alternative Buildable          628            0           0         0
            Build failed        73          215           0         0
            Deps failed          0            0         170         0
            Not tried            0            0           0         0
~~~~

These results mean that 73 packages became unbuildable when the
alternative layout rule is used.

## Caution

Hackager can cause arbitrary code to run on your machine. For example:
 * TemplateHaskell is run at compile time and can execute arbitrary
   code
 * Package configure scripts will be run
 * Custom Setup.hs programs will be run

