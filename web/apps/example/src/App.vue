<template>
  <v-app>
    <router-view />
  </v-app>
</template>

<script lang="ts" setup>
import { useAppStore } from "@/stores/app.ts";
import { loadTecLangWasm } from "@/teclang-wasm-load.ts";
import { createTecLangWasmEffect } from "@/teclang-wasm-effect.ts";

onMounted(async () => {
  const appStore = useAppStore();
  if (!appStore.haskell)
    appStore.setHaskell(createTecLangWasmEffect(await loadTecLangWasm()));
});
</script>
