<template>
  <v-app>
    <router-view />
  </v-app>
</template>

<script lang="ts" setup>
import {ConsoleStdout, File, OpenFile, WASI} from "@bjorn3/browser_wasi_shim";

onMounted(async () => {
  let args = ["bin", "arg1", "arg2"];
  let env = ["FOO=bar"];
  let fds = [
    new OpenFile(new File([])), // stdin
    ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ${msg}`)),
    ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ${msg}`)),
  ];
  let wasi = new WASI(args, env, fds);

  let wasm = await WebAssembly.compileStreaming(fetch("teclang.wasm"));
  let inst = await WebAssembly.instantiate(wasm, {
    wasi_snapshot_preview1: wasi.wasiImport,
  });
  wasi.start(inst as any);
});
</script>
