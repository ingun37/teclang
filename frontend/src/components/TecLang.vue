<script lang="ts" setup>
import { useAppStore } from "@/stores/app.ts";
import { ref } from "vue";
import { encodeTecAST, type TecAST } from "@/schema/TecAstSchema.ts";

const store = useAppStore();
const tecLangResult = ref<string | null>(null);
const loading = ref(false);
const props = defineProps<{ tecAst: TecAST }>();
async function fetchTecLang(): Promise<string> {
  const jsonStr = encodeTecAST(props.tecAst).trim();
  return store.wasmInstance!.makeHaskell(jsonStr);
}

async function handleShowTecLang() {
  loading.value = true;
  try {
    tecLangResult.value = await fetchTecLang();
  } catch (error) {
    console.error("Error fetching TecLang:", error);
    tecLangResult.value = "Error loading TecLang";
  } finally {
    loading.value = false;
  }
}
</script>

<template>
  <v-sheet class="d-flex flex-row ga-2 align-center">
    <v-btn :loading="loading" size="small" @click="handleShowTecLang">
      Show TecLang
    </v-btn>
    <span v-if="tecLangResult">{{ tecLangResult }}</span>
  </v-sheet>
</template>

<style lang="sass" scoped></style>
