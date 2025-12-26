<template>
  <v-app>
    <router-view />
  </v-app>
</template>

<script lang="ts" setup>
import { useAppStore } from "@/stores/app.ts";
import { loadTecLangWasm } from "@/teclang-wasm-load.ts";

onMounted(() => {
  const appStore = useAppStore();
  if (!appStore.wasmInstance) appStore.setWasmInstance(loadTecLangWasm());
});
</script>
