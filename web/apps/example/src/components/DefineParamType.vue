<script setup lang="ts">
import * as C from "codec";
const model = defineModel<string>({ required: true });
const primitiveOptions = ["String", "Number"];
const props = defineProps<{ enums: readonly C.TecType.TecEnum[] }>();
const options = computed<string[]>(() => {
  return primitiveOptions.concat(props.enums.map((e) => e.tecTypeName));
});
</script>

<template>
  <v-select
    v-model="model"
    :items="options"
    density="compact"
    hide-details
    variant="outlined"
    class="mb-2"
  />
</template>

<style scoped lang="sass"></style>
