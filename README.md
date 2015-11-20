# Hackager [![Hackage version](https://img.shields.io/hackage/v/hackager.svg?style=flat)](https://hackage.haskell.org/package/hackager) [![Build Status](https://img.shields.io/travis/dterei/hackager.svg?style=flat)](https://travis-ci.org/dterei/hackager)

Hackager is a tool to compile all of the Haskell Hackage package repository.
This is useful for testing Haskell compilers.

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
                       [-f PKG-FLAGS] [-n THREADS] [-r SEARCH] [PKGS...]

    NAME         A name by which the results of this hackager run will
                 be referred, e.g. \"ghc-7.6.1\".
    CABAL        The path to the cabal program to use.
    GHC          The path to the ghc program to use.
    GHC-PKG      The path to the ghc-pkg program to use.
    DEP-FLAGS    The flags to use when compiling dependencies of a package.
                 e.g. \"--ghc-option=-XFoo\"
    PKG-FLAGS    The flags to use when compiling a package.
                 e.g. \"--ghc-option=-XBar\"
    THREADS      Number of threads to use to build in parallel.
    SEARCH       A regular expression to use for selecting packages,
                 when used, don't specify a package list.
    PKGS         A list of packages to build. If not specified all of
                 hackage is built.
~~~~

### Executing a run of Hackager

Here is a run with GHC, no special options and using 4 threads (note that this
generally takes a long time, i.e. a few days):

~~~ {.sh}
$ hackager record -o normal -n 4
~~~~

This run has two parts. First, the 'stats' part, where Hackager checks which
packages of the ones requested it believes it can build. Packages that can't be
built are ones that we can't satisfy the dependencies for, usually due to the
package itself or one of its dependencies not being compatible with the version
of GHC in use. This produces files of the form `stats.*` in the output
directory and should only take a few minutes.

The second part consists of attempting to build every package (in isolation)
that Hackager reported it could attempt to build from the first part. This
takes hours to days (for all of Hackage), and stores results in files of the
form `build.*`. Log files for the build results of each package are also saved
under folders (with alphabetical grouping to make browsing easier).

### Comparing Results of Two Runs

After the first fun, execute a second run with the delta you wish. For example,
this time using ''-XAlternativeLayoutRule'' to compile each package (but not
the dependencies of the package):

~~~~ {.sh}
$ hackager record -o altern -f "--ghc-option=-XAlternativeLayoutRule" -n 4
~~~~

Once done, you can compare the results of the two runs:

~~~~ {.sh}
$ hackager report normal altern

                            normal
                     Built, Failed, Deps Failed, Not Tried
altern Built           628       0            0          0
       Failed           73     215            0          0
       Deps Failed       0       0          170          0
       Not Tried         0       0            0          0
~~~~

These results mean that 73 packages became unbuildable when the alternative
layout rule is used.

## File Output

When looking at the files created by a single run of Hackager, the important
one is `stats.summary`, which cotains the following fields:

* ''Num packages'':   Number of packges we are testing.
* ''Installable'':    Packages we believe we can install.
* ''Uninstallable'':  Packages we can't build (i.e., wrong GHC version)
* ''Errored '':       Packages Cabal claims can be built but we don't
                      understand Cabal's output.
* ''Installations'':  Total number of packages builds we will perform during
                      the run.

Hackager also produces a reverse dependency list for each package and a
histogram of the reverse dependency count for pacakges, storing them in files
of the form `stats.*`.

## Caution

Hackager can cause arbitrary code to run on your machine. For example:

 * TemplateHaskell is run at compile time and can execute arbitrary code
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

