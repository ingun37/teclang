<script lang="ts" setup>
import type { TecAST } from "@/schema/TecAstSchema.ts";
import TecType from "@/components/TecType.vue";

interface Props {
  ast: TecAST;
}

const props = defineProps<Props>();
const emit = defineEmits<{
  deleted: [TecAST];
}>();
function onDelete() {
  emit("deleted", props.ast);
}
</script>

<template>
  <v-sheet v-if="ast.tag === 'TecType'">
    <TecType :input-tec-type="ast" @deleted="onDelete" />
  </v-sheet>
  <v-sheet v-if="ast.tag === 'TecQuery'">
    <Query :query="ast" />
  </v-sheet>
</template>

<style lang="sass" scoped></style>
