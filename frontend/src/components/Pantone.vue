<script lang="ts" setup>
import type { Pantone } from "@/schema/TecRefined.ts";
import { useAppStore } from "@/stores/app.ts";
import { iterateIndexSetsDB } from "@/schema/IterateTec.ts";

interface Props {
  readonly item: Pantone;
}
const props = defineProps<Props>();

const matrix = computed(() => {
  const db = useAppStore().graphDB;
  const renderItem: Pantone = props.item;
  return Array.from(
    iterateIndexSetsDB(db, renderItem.typeName, renderItem.parameters),
  );
});
</script>

<template>
  <div v-for="(item, index) in matrix" :key="index" class="pantone-container">
    <div class="color-square"></div>
    <div class="color-label">PANTONE A7@h</div>
  </div>
</template>

<style lang="sass" scoped>
.pantone-container
  display: flex
  flex-direction: column
  align-items: center
  gap: 8px

.color-square
  width: 60px
  height: 60px
  background-color: #40E0D0
  border: 1px solid #ccc
  border-radius: 4px

.color-label
  font-size: 14px
  text-align: center
</style>
