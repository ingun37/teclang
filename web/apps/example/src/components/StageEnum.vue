<script setup lang="ts">
import * as C from "codec";
import { useAppStore } from "@/stores/app.ts";
defineEmits<{
  (e: "update", value: C.TecEnum.TecEnumAST): void;
}>();
const tecEnumAST = ref<C.TecEnum.TecEnumAST | null>(null);

const haskellCode = ref("");

const sampleHaskellCode = `\
data Side = Front
          | Back
          | Left
          | Right

data Article = A0 | A1

data Size = XS | S | M | L | XL | XXL

data Pattern = Pattern1 | Pattern2 | Pattern3 | Pattern4
data Line = Line1 | Line2 | Line3 | Line4{pattern :: Pattern}
`;

function fillSampleCode() {
  haskellCode.value = sampleHaskellCode;
}
async function updateUI() {
  tecEnumAST.value = C.jsonToTecEnumAST(
    JSON.parse(
      await useAppStore().haskell!.exports.encodeHaskellEnum(haskellCode.value),
    ),
  );
}

async function updateCode() {
  if (tecEnumAST.value)
    haskellCode.value = await useAppStore().haskell!.exports.decodeHaskellEnum(
      JSON.stringify(C.tecEnumASTToJson(tecEnumAST.value)),
    );
}
</script>

<template>
  <v-container fluid>
    <v-row>
      <v-col cols="12">
        <div class="d-flex flex-row ga-2 mb-4">
          <v-btn @click="fillSampleCode">fill preset schema - clo3d</v-btn>
        </div>
      </v-col>

      <v-col cols="12">
        <v-textarea
          v-model="haskellCode"
          auto-grow
          class="font-mono"
          label="TecLang"
          variant="outlined"
        />
      </v-col>

      <v-col cols="12">
        <div class="d-flex ga-2">
          <v-btn color="primary" @click="updateUI">update ui</v-btn>
          <v-btn color="primary" @click="updateCode">update code</v-btn>
        </div>
      </v-col>
      <v-col cols="12">
        <TecEnumAST v-if="tecEnumAST" v-model="tecEnumAST" />
      </v-col>
      <v-col cols="12" v-if="tecEnumAST">
        <v-btn
          color="primary"
          @click="$emit('update', tecEnumAST)"
          style="width: 100%"
          >GO!</v-btn
        >
      </v-col>
    </v-row>
  </v-container>
</template>

<style scoped lang="sass"></style>
