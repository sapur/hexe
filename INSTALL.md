# Installation Guide

`hexe` is implemented in Haskell. It contains a *Cabal* description.

## Preparations

Install the following tools:

 * `git`.
 * `ghc`, the *Glasgow Haskell Compiler*
 * `cabal-install`

`cabal-install` will download and compile all necessary files.

## A: Compile without sandbox

    git clone https://github.com/sapur/hexe.git
    cd hexe.git
    cabal install

By default, the binary will be installed to `.cabal/bin/hexe`. Add `.cabal/bin` 
to your `$PATH`.

## B: Compile with sandbox

In case the requirements of this package conflict with any other Cabal packages 
you might have installed, build this application in a sandbox:

    git clone https://github.com/sapur/hexe.git
    cd hexe.git
    cabal sandbox init
    cabal install

The binary will be at `dist/build/hexe/hexe` in the current directory. If you 
have `.local/bin` in your `$PATH`, you may want to put a symlink there (run 
from the root of the repository):

    ln -s $(readlink -f dist/build/hexe/hexe) .local/bin/
