<script setup lang="ts">
import * as C from "codec";
const model = defineModel<C.TecType.TecType[]>({ required: true });
function makeDefaultValue(): C.TecType.TecType {
  return C.TecType.TecType.make({ tecTypeName: "Default", classes: [] });
}
function addTecType() {
  model.value.push(makeDefaultValue());
}
</script>

<template>
  <DefineTecType v-for="(_, i) in model" v-model="model[i]!" />

  <v-btn prepend-icon="mdi-plus" variant="tonal" @click="addTecType">
    Add
  </v-btn>
</template>

<style scoped lang="sass"></style>
