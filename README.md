Hackager
========

Caution
-------
This program can cause arbitrary code to run on your machine. For
example:
* if any modules use TemplateHaskell
* Configure scripts will be run
* Custom Setup.hs programs will be run

Using
-----
Here's a run with the HEAD, no special options:

$ ht normal ../cabal-install/dist/build/cabal/cabal  \
            /home/ian/ghc/darcs/ghc/inst/bin/ghc     \
            /home/ian/ghc/darcs/ghc/inst/bin/ghc-pkg \
            "" ""

[7 hours pass]

Then another run, this time using -XAlternativeLayoutRule to compile
each package (but not the build dependencies of the package):

$ ht alternative ../cabal-install/dist/build/cabal/cabal  \
                 /home/ian/ghc/darcs/ghc/inst/bin/ghc     \
                 /home/ian/ghc/darcs/ghc/inst/bin/ghc-pkg \
                 ""                                       \
                 "--ghc-option=-XAlternativeLayoutRule"
[7 hours pass]

And finally a comparison of the results:

$ htc normal alternative
                            normal
                         Buildable Build failed Deps failed Not tried
alternative Buildable          628            0           0         0
            Build failed        73          215           0         0
            Deps failed          0            0         170         0
            Not tried            0            0           0         0

i.e. 73 packages became unbuildable when the alternative layout rule was
used.

