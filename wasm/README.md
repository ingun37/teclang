https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta
c84251ff51f503b0f7a0c357b7bbc80e009b74dc

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html#the-javascript-api

## wasm build
```sh
nix shell 'gitlab:haskell-wasm/ghc-wasm-meta/c84251ff51f503b0f7a0c357b7bbc80e009b74dc?host=gitlab.haskell.org'#all_9_12

# build wasm
wasm32-wasi-cabal build

# run test
cabal test
```

## front build

```sh
cp ../wasm/dist-newstyle/build/wasm32-wasi/ghc-9.12.2.20251128/teclang-0.1.0.0/x/teclang/build/teclang/teclang.wasm ./public/
```