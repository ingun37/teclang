<script setup lang="ts">
import * as C from "codec";
const model = defineModel<C.TecType.TecType>({ required: true });
function makeDefaultValue(): C.TecType.TecSum {
  return C.TecType.TecSum.make({ tecTypeName: "Default", classes: [] });
}
function addTecType() {
  model.value = C.TecType.TecType.make({
    sumTypes: [...model.value.sumTypes, makeDefaultValue()],
  });
}
</script>

<template>
  <v-container fluid>
    <v-row>
      <v-col v-for="(_, i) in model.sumTypes" :key="i" cols="12">
        <DefineTecType v-model="model.sumTypes[i]!" />
      </v-col>

      <v-col cols="12">
        <div class="d-flex flex-row ga-2">
          <v-btn prepend-icon="mdi-plus" variant="tonal" @click="addTecType">
            Add
          </v-btn>
        </div>
      </v-col>
    </v-row>
  </v-container>
</template>

<style scoped lang="sass"></style>
