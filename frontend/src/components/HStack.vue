<script lang="ts" setup>
import TecAST from "@/components/TecAST.vue";
import type { TecAST as TecASTType } from "@/schema/TecAstSchema.ts";

interface Props {
  items: readonly TecASTType[];
}

const props = defineProps<Props>();
</script>

<template>
  <v-sheet class="d-flex flex-row ga-1">
    <v-sheet v-for="(item, index) in items" :key="index">
      <Resizable>
        <v-spacer
          v-if="item.tag === 'TecType' && item.typeName === 'Spacing'"
          style="min-width: 100px"
        ></v-spacer>
        <TecAST v-else :ast="item" />
      </Resizable>
    </v-sheet>
  </v-sheet>
</template>

<style lang="sass" scoped></style>
