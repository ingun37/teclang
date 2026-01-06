<script setup lang="ts">
import * as C from "codec";
import { useAppStore } from "@/stores/app.ts";
defineProps<{ tecEnumAst: C.TecEnum.TecEnumAST }>();

defineEmits<{
  (e: "update", value: C.TecClass.TecClassAST): void;
}>();
const tecClassAST = ref<C.TecClass.TecClassAST | null>(null);

const haskellCode = ref("");

const sampleHaskellCode = `\
line :: Line -> Pattern
render :: Side -> Article -> Image
pom :: Side -> Line -> Number
schematic :: Side -> Image
`;

function fillSampleCode() {
  haskellCode.value = sampleHaskellCode;
}
async function updateUI() {
  tecClassAST.value = C.jsonToTecClassAST(
    JSON.parse(
      await useAppStore().haskell!.exports.encodeHaskellClass(
        haskellCode.value,
      ),
    ),
  );
}

async function updateCode() {
  if (tecClassAST.value)
    haskellCode.value = await useAppStore().haskell!.exports.decodeHaskellClass(
      JSON.stringify(C.tecClassASTToJson(tecClassAST.value)),
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
        <TecClassAST
          v-if="tecClassAST"
          v-model="tecClassAST"
          :tec-enum-ast="tecEnumAst"
        />
      </v-col>
      <v-col cols="12" v-if="tecClassAST">
        <v-btn
          color="primary"
          @click="$emit('update', tecClassAST)"
          style="width: 100%"
          >GO!</v-btn
        >
      </v-col>
    </v-row>
  </v-container>
</template>

<style scoped lang="sass"></style>
