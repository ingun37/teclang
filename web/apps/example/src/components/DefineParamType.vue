<script setup lang="ts">
const model = defineModel<string>({ required: true });
const options = ["String", "Number"];
</script>

<template>
  <v-select
    v-model="model"
    :items="options"
    label="Parameter Type"
    density="compact"
    hide-details
    variant="outlined"
    class="mb-2"
  />
</template>

<style scoped lang="sass"></style>
