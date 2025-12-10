<script lang="ts" setup>
import type { TecAST } from "@/schema/TecAstSchema.ts";
import TecType from "@/components/TecType.vue";

interface Props {
  ast: TecAST;
}

const props = defineProps<Props>();
</script>

<template>
  <v-card class="mb-2">
    <v-card-text>
      <v-sheet v-if="ast.tag === 'TecType'">
        <TecType :tec-type="ast" />
      </v-sheet>
    </v-card-text>
  </v-card>
</template>

<style lang="sass" scoped></style>
