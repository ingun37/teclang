<script setup lang="ts">
import * as C from "codec";
import DefineClasses from "@/components/DefineClasses.vue";

const model = defineModel<C.TecType.TecSum>({ required: true });
</script>

<template>
  <DefineClasses v-model="model.classes" />
</template>

<style scoped lang="sass"></style>
