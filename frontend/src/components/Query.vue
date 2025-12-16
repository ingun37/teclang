<script lang="ts" setup>
import { useAppStore } from "@/stores/app.ts";
import type { TecQuery } from "@/schema/TecAstSchema.ts";
import { iterateQuery } from "@/schema/IterateTec.ts";
import { Array } from "effect";

const props = defineProps<{ query: TecQuery }>();
const store = useAppStore();

const items = computed(() => {
  const db = store.graphDB;

  return [...iterateQuery(db, props.query)];
});
</script>
<template>
  <v-sheet v-if="Array.isNonEmptyArray(items)">
    <EntryMatrix :entries="items" axis="column"></EntryMatrix>
  </v-sheet>
  <div v-else class="no-results">No results</div>
</template>

<style lang="sass" scoped>

.no-results
  color: #999
  font-style: italic
</style>
