<template>
  <div class="d-flex flex-row ga-2">
    <v-btn @click="fillSampleCode">fill sample code</v-btn>
    <v-btn @click="updateUI">update ui</v-btn>
  </div>
  <v-textarea
    v-model="haskellCode"
    auto-grow
    class="font-mono"
    label="Haskell Code"
  />
  <DefineTecTypes v-if="tecType" v-model="tecType" />
</template>

<script lang="ts" setup>
import * as C from "codec";
import { useAppStore } from "@/stores/app.ts";
const tecType = ref<C.TecType.UniqueTecType | null>(null);

const haskellCode = ref("");

const sampleHaskellCode = `\
data Side = Front
          | Back
          | Left
          | Right
data Article = A0 | A1
data TecType = Render Side Article`;

function fillSampleCode() {
  haskellCode.value = sampleHaskellCode;
}
async function updateUI() {
  tecType.value = C.jsonToUniqueTecType(
    JSON.parse(
      await useAppStore().haskell!.exports.encodeHaskellType(haskellCode.value),
    ),
  );
}

watch(tecType, async (newTecType) => {
  if (newTecType)
    haskellCode.value = await useAppStore().haskell!.exports.decodeHaskellType(
      JSON.stringify(C.uniqueTecTypeToJson(newTecType)),
    );
});
</script>
