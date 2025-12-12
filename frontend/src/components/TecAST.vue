<script lang="ts" setup>
import type { TecAST } from "@/schema/TecAstSchema.ts";
import TecType from "@/components/TecType.vue";

interface Props {
  ast: TecAST;
}

const props = defineProps<Props>();
const emit = defineEmits<{
  deleted: [TecAST];
  updated: [TecAST];
}>();
function onDelete() {
  console.log("Delete");
  emit("deleted", props.ast);
}
</script>

<template>
  <v-sheet v-if="ast.tag === 'TecType'">
    <TecType
      :input-tec-type="ast"
      @deleted="onDelete"
      @updated="(x) => emit('updated', x)"
    />
  </v-sheet>
  <v-sheet v-if="ast.tag === 'TecQuery'">
    <Query :query="ast" />
  </v-sheet>
  <v-sheet v-if="ast.tag === 'TecBinding'">
    <TecBinding :binding="ast" @updated="(x) => emit('updated', x)" />
  </v-sheet>
  <v-sheet v-if="ast.tag === 'TecVar'"> variable:{{ ast.varName }} </v-sheet>
  <v-sheet v-if="ast.tag === 'TecInt'">{{ ast.int }}</v-sheet>
  <v-sheet v-if="ast.tag === 'TecStr'">{{ ast.str }}</v-sheet>
</template>

<style lang="sass" scoped></style>
