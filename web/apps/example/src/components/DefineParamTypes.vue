<script setup lang="ts">
import DefineParamType from "@/components/DefineParamType.vue";

const model = defineModel<readonly string[]>({ required: true });

function addParamType() {
  model.value = model.value.concat(["NewType"]);
}
</script>

<template>
  <div class="ml-4 mb-4">
    <div class="text-caption mb-2">Parameter Types</div>
    <DefineParamType v-for="(_, i) in model" v-model="model[i]!" :key="i" />

    <v-btn
      prepend-icon="mdi-plus"
      variant="plain"
      size="small"
      @click="addParamType"
    >
      Add Parameter Type
    </v-btn>
  </div>
</template>

<style scoped lang="sass"></style>
