<script setup lang="ts">
import * as C from "codec";
import DefineParamTypes from "@/components/DefineParamTypes.vue";
const model = defineModel<C.TecType.TecClass>({ required: true });

function updateClassName(newName: string) {
  model.value = C.TecType.TecClass.make({ ...model.value, className: newName });
}
</script>

<template>
  <v-card>
    <v-card-text>
      <v-text-field
        v-model="model.className"
        label="Class Name"
        compact
        hide-details
        density="compact"
        class="mb-2"
        variant="underlined"
      />
      <DefineParamTypes v-model="model.parameterTypes" />
    </v-card-text>
  </v-card>
</template>

<style scoped lang="sass"></style>
