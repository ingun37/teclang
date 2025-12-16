<script lang="ts" setup>
import { useAppStore } from "@/stores/app.ts";
import { TecQuery } from "@/schema/TecAstSchema.ts";
import { iterateQuery } from "@/schema/IterateTec.ts";
import { Array } from "effect";

const props = defineProps<{ query: TecQuery }>();
const store = useAppStore();

const items = computed(() => {
  const db = store.graphDB;

  return [...iterateQuery(db, props.query)];
});

const emit = defineEmits<{
  updated: [TecQuery];
}>();
function rotate(q: TecQuery): TecQuery {
  if (q.left.tag === "TecQuery") {
    const rotatedL = rotate(q.left);
    return TecQuery.make({
      op: q.op,
      left: TecQuery.make({ op: q.op, left: rotatedL.left, right: q.right }),
      right: rotatedL.right,
    });
  } else {
    return TecQuery.make({ op: q.op, left: q.right, right: q.left });
  }
}
</script>
<template>
  <v-btn @click="emit('updated', rotate(query))">rotate query operands</v-btn>
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
