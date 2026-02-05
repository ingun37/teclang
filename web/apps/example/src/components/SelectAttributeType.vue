<script setup lang="ts">
import * as C from "codec";
const model = defineModel<C.TecClass.TecClassAttribute>({ required: true });
defineEmits<{
  delete: [];
}>();
</script>

<template>
  <v-select
    v-model="model"
    :items="['String', 'Number', 'Image', 'Color']"
    density="compact"
    hide-details
    variant="outlined"
    append-icon="mdi-delete"
    @click:append="$emit('delete')"
  />
</template>

<style scoped lang="sass"></style>
