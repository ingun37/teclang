<script lang="ts" setup>
import { useAppStore } from "@/stores/app.ts";
import type { TecQuery } from "@/schema/TecAstSchema.ts";
import { iterateQuery } from "@/schema/IterateTec.ts";

const props = defineProps<{ query: TecQuery }>();
const store = useAppStore();

const items = computed(() => {
  const db = store.graphDB;

  return [...iterateQuery(db, props.query)];
});
</script>
<template>
  <v-sheet v-if="items" class="query-results">
    <v-sheet
      v-for="(queryEntries, index) in items"
      :key="index"
      class="query-item"
    >
      <v-sheet v-for="(queryEntry, index) in queryEntries" :key="index">
        <Single :t-entry="queryEntry" />
      </v-sheet>
    </v-sheet>
  </v-sheet>
  <div v-else class="no-results">No results</div>
</template>

<style lang="sass" scoped>
.query-results
  display: flex
  flex-direction: column
  gap: 8px

.query-item
  padding: 8px
  background-color: #f5f5f5
  border-radius: 4px
  border: 1px solid #ddd

.no-results
  color: #999
  font-style: italic
</style>
