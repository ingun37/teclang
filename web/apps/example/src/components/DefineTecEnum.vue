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
  <v-card class="pa-4">
    <v-card-text>
      <v-text-field
        v-model="model.tecTypeName"
        label="Name"
        density="compact"
      />

      <div class="d-flex flex-column ga-2 align-center">
        <div v-for="(_, index) in model.values" :key="index" style="width: 7em">
          <v-text-field
            v-model="model.values[index]"
            :label="`Value ${index}`"
            density="compact"
            hide-details
          />
        </div>
        <v-btn
          prepend-icon="mdi-plus"
          variant="plain"
          size="small"
          @click="addValue"
        >
          Add Value
        </v-btn>
      </div>
    </v-card-text>
  </v-card>
</template>

<style scoped lang="sass"></style>
