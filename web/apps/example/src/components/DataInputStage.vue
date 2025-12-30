<script setup lang="ts">
import * as C from "codec";
import InputClass from "@/components/InputClass.vue";
defineProps<{ tecSchema: C.TecType.TecSchema }>();
</script>

<template>
  <div v-for="tecClass in tecSchema.tecSum.classes" :key="tecClass.className">
    <InputClass :tec-class="tecClass" :tec-enums="tecSchema.tecEnums" />
  </div>
</template>

<style scoped lang="sass"></style>
