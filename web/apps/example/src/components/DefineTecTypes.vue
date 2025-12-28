<script setup lang="ts">
import * as C from "codec";
import DefineTecEnum from "@/components/DefineTecEnum.vue";
const model = defineModel<C.TecType.UniqueTecType>({ required: true });
function makeDefaultEnum(): C.TecType.TecEnum {
  return C.TecType.TecEnum.make({ tecTypeName: "Default", values: [] });
}
function addTecType() {
  model.value = C.TecType.UniqueTecType.make({
    enums: [...model.value.enums, makeDefaultEnum()],
    tecType: model.value.tecType,
  });
}
</script>

<template>
  <v-container fluid class="pa-0">
    <v-row>
      <v-col v-for="(_, i) in model.enums" :key="i" cols="6">
        <DefineTecEnum v-model="model.enums[i]!" />
      </v-col>
    </v-row>
    <v-row>
      <v-col cols="12">
        <DefineTecType v-model="model.tecType" />
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
