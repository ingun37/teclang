<template>
  <v-app>
    <router-view />
  </v-app>
</template>

<script lang="ts" setup>
import { useAppStore } from "@/stores/app.ts";
import { loadTecLangWasm } from "@/teclang-wasm-load.ts";
import { createTecLangWasmEffect } from "@/teclang-wasm-effect.ts";
import initSqlJs from "sql.js";
import sqlWasmUrl from "sql.js/dist/sql-wasm.wasm?url";

onMounted(async () => {
  const appStore = useAppStore();
  if (!appStore.haskell)
    appStore.setHaskell(createTecLangWasmEffect(await loadTecLangWasm()));
  if (!appStore.db) {
    const SQL = await initSqlJs({
      locateFile: () => sqlWasmUrl,
    });
    appStore.setDB(new SQL.Database());
  }
});
</script>
