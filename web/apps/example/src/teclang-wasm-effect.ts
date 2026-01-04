import { type HaskellWasm } from "./teclang-wasm-load.js";
export function createTecLangWasmEffect(exports: HaskellWasm) {
  return {
    exports,
  };
}

export type HaskellEffect = ReturnType<typeof createTecLangWasmEffect>;
