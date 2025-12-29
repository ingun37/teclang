<script setup lang="ts">
import * as C from "codec";
import DefineClasses from "@/components/DefineClasses.vue";

const model = defineModel<C.TecType.TecSum>({ required: true });
const props = defineProps<{ tecEnums: readonly C.TecType.TecEnum[] }>();
</script>

<template>
  <DefineClasses v-model="model.classes" :tecEnums="props.tecEnums" />
</template>

<style scoped lang="sass"></style>
