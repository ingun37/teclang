<script lang="ts" setup>
import TecAST from "@/components/TecAST.vue";
import type { TecAST as TecASTType } from "@/schema/TecAstSchema.ts";

interface Props {
  items: readonly TecASTType[];
}

const props = defineProps<Props>();
</script>

<template>
  <v-sheet class="d-flex flex-row">
    <TecAST v-for="(item, index) in items" :key="index" :ast="item" />
  </v-sheet>
</template>

<style lang="sass" scoped></style>
