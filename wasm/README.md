https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta
c84251ff51f503b0f7a0c357b7bbc80e009b74dc

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html#the-javascript-api

## wasm build

```sh
nix shell 'gitlab:haskell-wasm/ghc-wasm-meta/c84251ff51f503b0f7a0c357b7bbc80e009b74dc?host=gitlab.haskell.org'#all_9_12

./build.sh

# run test
cabal test
```

## front build

```sh
cp ../wasm/build/teclang-wasm.wasm ./public/
cp ../wasm/build/teclang-wasm.js ./src/teclang-wasm.js

pnpm run dev
```