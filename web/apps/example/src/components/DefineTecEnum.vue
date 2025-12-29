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
  <v-card>
    <v-card-text>
      <v-text-field
        v-model="model.tecTypeName"
        label="Name"
        density="compact"
      />

      <div class="d-flex flex-column ga-2 align-start">
        <div class="text-caption mb-2">Values</div>

        <v-text-field
          v-for="(_, index) in model.values"
          v-model="model.values[index]"
          density="compact"
          hide-details
          style="width: 6rem"
        />
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
