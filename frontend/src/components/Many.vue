<script lang="ts" setup>
import type { IndexSet } from "@/schema/TecRefined.ts";
import { useAppStore } from "@/stores/app.ts";
import { iterateIndexSetsDB } from "@/schema/IterateTec.ts";

const props = defineProps<{
  typeName: string;
  indexSets: readonly IndexSet[];
}>();

const matrix = computed(() => {
  const db = useAppStore().graphDB;
  return Array.from(iterateIndexSetsDB(db, props.typeName, props.indexSets));
});
</script>

<template>
  <v-sheet v-for="(entry, index) in matrix" :key="index">
    <Single :entry="entry" :typeName="typeName" />
  </v-sheet>
</template>

<style lang="sass" scoped></style>
