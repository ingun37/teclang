<template>
  <div class="d-flex flex-row ga-2">
    <v-btn @click="fillSampleCode">fill sample code</v-btn>
    <v-btn @click="updateTypeAST">update</v-btn>
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
data Enum0 = A
           | B
           | C

data TecType = AType Enum0 String`;

function fillSampleCode() {
  haskellCode.value = sampleHaskellCode;
}
async function updateTypeAST() {
  tecType.value = C.jsonToUniqueTecType(
    JSON.parse(
      await useAppStore().haskell!.exports.encodeHaskellType(haskellCode.value),
    ),
  );
}
</script>
