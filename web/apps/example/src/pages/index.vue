<template>
  <DefineTecTypes v-model="tecType" />
</template>

<script lang="ts" setup>
import * as C from "codec";
const tecType = ref<C.TecType.TecType>(
  C.TecType.TecType.make({
    sumTypes: [],
  }),
);
</script>
