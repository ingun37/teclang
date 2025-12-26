<template>
  <TecTypeDefinition v-model="tecTypes" />
</template>

<script lang="ts" setup>
import * as C from "codec";
const tecTypes = ref<C.TecType.TecType[]>([]);
</script>
