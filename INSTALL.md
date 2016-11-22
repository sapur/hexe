# Installation Guide

`hexe` is implemented in Haskell. It contains a *Cabal* description.

## Preparations

Install the following tools with your operating system's package manager:

 * `git`
 * `haskell-platform` ([link](https://www.haskell.org/downloads))

If the Haskell platform is not available for your system, you may be able to 
install the required packages individually, namely *Glasgow Haskell Compiler* (`ghc`) and either `stack` or `cabal-install`.

Then run:

    git clone https://github.com/sapur/hexe.git
    cd hexe.git

From here on, install `hexe` either via `stack`, or via `cabal-install`. Follow 
either of the following sections.

## A: Build with "stack"

From the repository root run:

    stack install

The binary will be symlinked to `.local/bin/hexe`. Add `.local/bin` to your 
`$PATH`.

## B: Build with "cabal-install" without a sandbox

    cd hexe.git
    cabal install

By default, the binary will be installed to `.cabal/bin/hexe`. Add `.cabal/bin` 
to your `$PATH`.

## C: Build with "cabal-install" in a sandbox

In case the requirements of this package conflict with any other Cabal packages 
you might have installed, build this application in a sandbox:

    cd hexe.git
    cabal sandbox init
    cabal install

The binary will be at `dist/build/hexe/hexe` in the current directory. If you 
have `.local/bin` in your `$PATH`, you may want to put a symlink there (run 
from the root of the repository):

    ln -s $(readlink -f dist/build/hexe/hexe) .local/bin/
