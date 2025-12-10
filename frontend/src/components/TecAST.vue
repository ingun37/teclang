<script lang="ts" setup>
import type { TecAST } from "@/schema/TecAstSchema.ts";
import TecType from "@/components/TecType.vue";

interface Props {
  ast: TecAST;
}

const props = defineProps<Props>();
</script>

<template>
  <v-sheet v-if="ast.tag === 'TecType'" min-width="100px">
    <TecType :tec-type="ast" />
  </v-sheet>
  <v-sheet v-if="ast.tag === 'TecQuery'">
    <Query :query="ast" />
  </v-sheet>
</template>

<style lang="sass" scoped></style>
