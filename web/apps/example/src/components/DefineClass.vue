<script setup lang="ts">
import * as C from "codec";
import DefineParamTypes from "@/components/DefineParamTypes.vue";
const model = defineModel<C.TecType.TecClass>({ required: true });
const intermediateName = ref(model.value.className);

function updateClassName(newName: string) {
  model.value = C.TecType.TecClass.make({ ...model.value, className: newName });
}
</script>

<template>
  <v-text-field
    v-model="intermediateName"
    compact
    hide-details
    density="compact"
    label="Class Name"
    class="mb-2"
    @keyup.enter="updateClassName(intermediateName)"
  >
    <template #append-inner>
      <v-btn
        icon="mdi-check"
        variant="text"
        density="compact"
        @click="updateClassName(intermediateName)"
      />
    </template>
  </v-text-field>
  <DefineParamTypes v-model="model.parameterTypes" />
</template>

<style scoped lang="sass"></style>
