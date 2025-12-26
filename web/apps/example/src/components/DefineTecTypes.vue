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
  <v-container fluid>
    <v-row>
      <v-col v-for="(_, i) in model" :key="i" cols="12">
        <DefineTecType v-model="model[i]!" />
      </v-col>

      <v-col cols="12">
        <v-btn prepend-icon="mdi-plus" variant="tonal" @click="addTecType">
          Add
        </v-btn>
      </v-col>
    </v-row>
  </v-container>
</template>

<style scoped lang="sass"></style>
