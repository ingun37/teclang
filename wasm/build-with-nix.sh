#! /bin/bash

nix shell 'gitlab:haskell-wasm/ghc-wasm-meta/c84251ff51f503b0f7a0c357b7bbc80e009b74dc?host=gitlab.haskell.org'#all_9_12 --command ./build.sh