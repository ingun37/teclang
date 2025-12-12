<script lang="ts" setup>
import type { TecAST as TecASTType } from "@/schema/TecAstSchema.ts";
import Stack from "@/components/Stack.vue";

interface Props {
  items: readonly TecASTType[];
}
const emit = defineEmits<{
  onItemRemove: [TecASTType, number];
  updated: [TecASTType, number];
  added: [TecASTType];
}>();
const props = defineProps<Props>();

function deleteItem(item: TecASTType, index: number) {
  emit("onItemRemove", item, index);
}
function updateItem(newItem: TecASTType, index: number) {
  emit("updated", newItem, index);
}
</script>

<template>
  <Stack
    :items="props.items"
    axis="Y"
    @added="(x) => emit('added', x)"
    @removed="(x, y) => deleteItem(x, y)"
    @updated="(x, y) => updateItem(x, y)"
  />
</template>

<style lang="sass" scoped>

.add-button
  padding: 0 !important
  font-size: 18px
  align-self: stretch
  background-color: #e0ed7a
</style>
