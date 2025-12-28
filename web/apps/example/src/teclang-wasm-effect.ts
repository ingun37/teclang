import { type HaskellWasm } from "./teclang-wasm-load.js";
import * as E from "effect";
export function createTecLangWasmEffect(exports: HaskellWasm) {
  return {
    exports,
    makeHaskellMany(jsonStrs: string[]) {
      const e = E.pipe(
        jsonStrs,
        E.Array.map((jsonStr) =>
          E.Effect.tryPromise({
            try: (signal) => {
              return exports.decodeHaskellType(jsonStr);
            },
            catch: (err) => {
              console.error(err);
              return new Error("Failed to make Haskell");
            },
          }),
        ),
        E.Effect.allWith({ concurrency: 1 }),
        // E.Effect.map((results) => results.join("\n")),
      );
      return E.Effect.runPromise(e);
    },
  };
}

export type HaskellEffect = ReturnType<typeof createTecLangWasmEffect>;
