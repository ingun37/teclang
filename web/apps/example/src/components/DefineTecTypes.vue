<script setup lang="ts">
import * as C from "codec";
import { useAppStore } from "@/stores/app.ts";
const model = defineModel<C.TecType.TecType[]>({ required: true });
const tecLang = ref("");
function makeDefaultValue(): C.TecType.TecType {
  return C.TecType.TecType.make({ tecTypeName: "Default", classes: [] });
}
function addTecType() {
  model.value.push(makeDefaultValue());
}
async function showTecLang() {
  const app = useAppStore();
  const jsonStrings = model.value
    .map(C.tecTypeToJson)
    .map((x) => JSON.stringify(x));
  const lines = await app.haskell!.makeHaskellMany(jsonStrings);
  tecLang.value = lines.join("\n");
}
</script>

<template>
  <v-container fluid>
    <v-row>
      <v-col v-for="(_, i) in model" :key="i" cols="12">
        <DefineTecType v-model="model[i]!" />
      </v-col>

      <v-col cols="12">
        <div class="d-flex flex-row ga-2">
          <v-btn prepend-icon="mdi-plus" variant="tonal" @click="addTecType">
            Add
          </v-btn>
          <v-btn variant="tonal" @click="showTecLang"> Show TecLang </v-btn>
        </div>
      </v-col>
      <v-col cols="12" v-if="tecLang">
        <v-code>
          {{ tecLang }}
        </v-code>
      </v-col>
    </v-row>
  </v-container>
</template>

<style scoped lang="sass"></style>
