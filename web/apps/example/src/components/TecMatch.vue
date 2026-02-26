<script setup lang="ts">
import * as C from "codec";
const model = defineModel<C.TecPnum.TecMatch>({ required: true });
const props = defineProps<{
  pnumAst: C.TecPnum.TecPnumAST;
  pnum: C.TecPnum.TecPnum;
}>();
</script>

<template></template>

<style scoped lang="sass"></style>
