import { ConsoleStdout, File, OpenFile, WASI } from "@bjorn3/browser_wasi_shim";
import wasm_wrapper from "./teclang-wasm.js";
export interface HaskellWasm {
  decodeHaskellData(jsonStr: string): Promise<string>;
  decodeHaskellEnum(jsonStr: string): Promise<string>;
  decodeHaskellClass(jsonStr: string): Promise<string>;
  decodeHaskellNode(jsonStr: string): Promise<string>;
  decodeHaskellQuery(jsonStr: string): Promise<string>;
  decodeHaskellPnum(jsonStr: string): Promise<string>;
  encodeHaskellData(jsonStr: string): Promise<string>;
  encodeHaskellEnum(jsonStr: string): Promise<string>;
  encodeHaskellClass(jsonStr: string): Promise<string>;
  encodeHaskellNode(jsonStr: string): Promise<string>;
  encodeHaskellQuery(jsonStr: string): Promise<string>;
  encodeHaskellPnum(jsonStr: string): Promise<string>;
  // formatHaskell(jsonStr: string): Promise<string>;
}
export async function loadTecLangWasm() {
  let args = ["bin", "arg1", "arg2"];
  let env = ["FOO=bar"];
  let fds = [
    new OpenFile(new File([])), // stdin
    ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ${msg}`)),
    ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ${msg}`)),
  ];
  let wasi = new WASI(args, env, fds);
  let __exports = {};
  let wasm = await WebAssembly.compileStreaming(fetch("teclang-wasm.wasm"));
  let inst = await WebAssembly.instantiate(wasm, {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: wasm_wrapper(__exports),
  });
  Object.assign(__exports, inst.exports);

  wasi.initialize(inst as any);
  const exports = inst.exports as any as HaskellWasm;
  (exports as any).hs_init(0, 0);
  (window as any).wasm = exports;
  return exports;
}
