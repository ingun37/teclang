set -xe

out_path="dist-newstyle/build/wasm32-wasi/ghc-9.12.2.20251128/teclang-0.1.0.0/x/teclang-wasm/build/teclang-wasm/teclang-wasm.wasm"

wasm32-wasi-cabal update

wasm32-wasi-cabal build

mkdir -p build

cp $out_path build/

$(wasm32-wasi-ghc --print-libdir)/post-link.mjs \
  -i "$out_path" \
  -o build/teclang-wasm.js