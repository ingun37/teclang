<template>
  <v-app>
    <router-view />
  </v-app>
</template>

<script lang="ts" setup>
import { useAppStore } from "@/stores/app";

import { ConsoleStdout, File, OpenFile, WASI } from "@bjorn3/browser_wasi_shim";
import wasm_wrapper from "./teclang-wasm";

onMounted(async () => {
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
  const exports = inst.exports as any;
  exports.hs_init(0, 0);

  const appStore = useAppStore();
  appStore.setWasmInstance(exports);
  (window as any).wasm = exports;
});
</script>
