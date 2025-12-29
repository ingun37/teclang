<script lang="ts" setup>
import * as C from "codec";
import { useAppStore } from "@/stores/app.ts";
const emit = defineEmits<{
  (e: "update", value: C.TecType.TecSchema): void;
}>();
const tecType = ref<C.TecType.TecSchema | null>(null);

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
  tecType.value = C.jsonToTecSchema(
    JSON.parse(
      await useAppStore().haskell!.exports.encodeHaskellType(haskellCode.value),
    ),
  );
}

async function updateCode() {
  if (tecType.value)
    haskellCode.value = await useAppStore().haskell!.exports.decodeHaskellType(
      JSON.stringify(C.tecSchemaToJson(tecType.value)),
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
          label="Haskell Code"
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
        <DefineTecSchema v-if="tecType" v-model="tecType" />
      </v-col>
      <v-col cols="12" v-if="tecType">
        <v-btn
          color="primary"
          @click="emit('update', tecType)"
          style="width: 100%"
          >GO!</v-btn
        >
      </v-col>
    </v-row>
  </v-container>
</template>

<style scoped lang="sass"></style>
