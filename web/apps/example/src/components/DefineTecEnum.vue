<script setup lang="ts">
import * as C from "codec";
const model = defineModel<C.TecType.TecEnum>({ required: true });

function addValue() {
  model.value = {
    ...model.value,
    values: [...model.value.values, "Foo"],
  };
}
</script>

<template>
  <v-container fluid class="pa-0">
    <v-row>
      <v-col cols="12">
        <v-text-field
          v-model="model.tecTypeName"
          label="Enum Name"
          variant="outlined"
          density="compact"
        />
      </v-col>
    </v-row>

    <div class="d-flex flex-row flex-wrap ga-2 align-center">
      <div v-for="(_, index) in model.values" :key="index" style="width: 7em">
        <v-text-field
          v-model="model.values[index]"
          :label="`Value ${index}`"
          variant="outlined"
          density="compact"
          hide-details
        />
      </div>
      <v-btn
        icon="mdi-plus"
        variant="tonal"
        size="small"
        color="primary"
        @click="addValue"
      />
    </div>
  </v-container>
</template>

<style scoped lang="sass"></style>
