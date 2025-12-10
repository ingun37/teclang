<script lang="ts" setup>
import type { Render } from "@/schema/TecRefined.ts";
import { useAppStore } from "@/stores/app.ts";
import { iterateIndexSetsDB } from "@/schema/IterateTec.ts";

interface Props {
  renderItem: Render;
}
const props = defineProps<Props>();
const matrix = computed(() => {
  const db = useAppStore().graphDB;
  const renderItem = props.renderItem;
  return Array.from(
    iterateIndexSetsDB(db, renderItem.typeName, renderItem.parameters),
  );
});
</script>

<template>
  <v-img
    v-for="(item, index) in matrix"
    :key="index"
    :src="`/render/${item.indexSet[0]}-${item.indexSet[1]}.png`"
    alt="Render image"
    max-width="500px"
    min-width="100px"
  />
</template>
<style lang="sass" scoped></style>
