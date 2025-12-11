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
  <v-sheet v-if="items">
    <v-sheet class="d-flex flex-row">
      <v-sheet v-for="(queryEntries, index) in items" :key="index">
        <v-sheet class="d-flex flex-column">
          <v-sheet v-for="(queryEntry, index) in queryEntries" :key="index">
            <Single :t-entry="queryEntry" class="pa-1" />
          </v-sheet>
        </v-sheet>
      </v-sheet>
    </v-sheet>
  </v-sheet>
  <div v-else class="no-results">No results</div>
</template>

<style lang="sass" scoped>

.no-results
  color: #999
  font-style: italic
</style>
