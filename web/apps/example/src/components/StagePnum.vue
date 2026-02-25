<script setup lang="ts">
import * as C from "codec";
import { useAppStore } from "@/stores/app.ts";
function createInitialPnum(): C.TecPnum.TecPnumAST {
  return C.TecPnum.TecPnumAST.make({
    tecPnums: [],
  });
}
const tecPnumAst = ref<C.TecPnum.TecPnumAST | null>(null);
const tecLangCode = ref("");
function fillSample() {
  tecLangCode.value = `\
data X = X0
       | X1

data Y = Y0
       | Y1

data E = E0
       | E1
       | E2

e :: X -> Y -> E
e X0 Y0 = E0
e _ _ = E1`;
}
async function updateUI() {
  tecPnumAst.value = C.jsonToTecPnumAST(
    JSON.parse(
      await useAppStore().haskell!.exports.encodeHaskellPnum(tecLangCode.value),
    ),
  );
}

async function updateCode() {
  if (tecPnumAst.value)
    tecLangCode.value = await useAppStore().haskell!.exports.decodeHaskellPnum(
      JSON.stringify(C.tecPnumASTToJson(tecPnumAst.value)),
    );
}
</script>

<template>
  <v-container fluid>
    <v-row>
      <v-col cols="12">
        <div class="d-flex flex-row ga-2 mb-4">
          <v-btn @click="fillSample">fill sample</v-btn>
        </div>
      </v-col>

      <v-col cols="12">
        <v-textarea
          v-model="tecLangCode"
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
        <TecPnumAst v-if="tecPnumAst" v-model="tecPnumAst" />
      </v-col>
      <v-col cols="12" v-if="tecPnumAst">
        <v-btn
          color="primary"
          @click="$emit('update', tecPnumAst)"
          style="width: 100%"
          >GO!</v-btn
        >
      </v-col>
    </v-row>
  </v-container>
</template>

<style scoped lang="sass"></style>
