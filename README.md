# Hackager

Hackager is a tool to compile all of the Haskell Hackage package
repository. This is useful for testing Haskell compilers.

## Using
Hackager consists of one tool that supports multiple commands.

''hackager'' is invoked with the following options:

~~~~ {.sh}
$ usage: hackager [--version] [--help] <command> [<args>]

The valid hackager commands are:
    record    Try building all of hackage and record results
    report    Compare two 'record' runs and display results

See 'hackager help <command>' for more information on a specific command
~~~~

''hackager record'' has the following options:

~~~~ {.sh}
usage: hackager record -o NAME [-c CABAL] [-g GHC] [-p GHC-PKG] [-d DEP-FLAGS]
                      [-f PKG-FLAGS] [-n THREADS] [PKGS...]

    NAME         A name by which the results of this hackager run will
                 be referred, e.g. \"ghc-7.6.1\"
    CABAL        The path to the cabal program to use
    GHC          The path to the ghc program to use
    GHC-PKG      The path to the ghc-pkg program to use
    DEP-FLAGS    The flags to use when compiling dependencies of a package
                 e.g. \"--ghc-option=-XFoo\"
    PKG-FLAGS    The flags to use when compiling a package
                 e.g. \"--ghc-option=-XBar\"
    THREADS      Number of threads to use to build in parallel
    PKGS         A list of packages to build. If not specified all of
                 hackage is built
~~~~

For example, here is a run with GHC, no special options and using 4
threads (note that this generally takes a long time, i.e. a few days):

~~~ {.sh}
$ hackager record -o normal -n 4
~~~~

Then another run, this time using ''-XAlternativeLayoutRule'' to
compile each package (but not the build dependencies of the package):

~~~~ {.sh}
$ hackager record -o altern -f "--ghc-option=-XAlternativeLayoutRule" -n 4
~~~~

And finally a comparison of the results:

~~~~ {.sh}
$ hackager report normal altern

                            normal
                         Buildable Build failed Deps failed Not tried
altern Buildable          628            0           0         0
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

## Get involved!

We are happy to receive bug reports, fixes, documentation
enhancements, and other improvements.

Please report bugs via the
[github issue tracker](http://github.com/dterei/Hackager/issues).

Master [git repository](http://github.com/dterei/Hackager):

* `git clone git://github.com/dterei/Hackager.git`

## Licensing

This library is BSD-licensed.
