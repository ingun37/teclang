<script lang="ts" setup>
import type { TecQuery, TecType } from "@/schema/TecAST.ts";
import { useAppStore } from "@/stores/app.ts";
import type Graph from "graphology";
import { HashSet } from "effect";

const props = defineProps<{ query: TecQuery }>();
const store = useAppStore();
function* iterateIDs(db: Graph, tt: TecType) {
  if (tt.index.tag === "IndexR") {
    const end = tt.index.to?.value ?? 10;

    for (let i = tt.index.from.value; i < end; i++) {
      if (tt.index1) {
        throw new Error("Not implemented");
      } else {
        const id = `${tt.typeName}-${i}`;
        if (!db.hasNode(id)) break;
        yield id;
      }
    }
  } else if (tt.index.tag === "IndexS") {
    const id = `${tt.typeName}-${tt.index.name}`;
    if (!db.hasNode(id)) return;
    yield id;
  }
}
function* recurse(query: TecQuery) {
  const db = store.graphDB;
  const left = query.left;
  const right = query.right;
  if (left.tag === "TecType" && right.tag === "TecType") {
    for (const leftID of iterateIDs(db, left)) {
      const neighbors = HashSet.fromIterable(db.directedNeighbors(leftID));
      const rightIDs = HashSet.fromIterable(iterateIDs(db, right));
      const intersection = HashSet.intersection(neighbors, rightIDs);
      for (const rightID of intersection) {
        yield [leftID, rightID];
      }
    }
  }
}
const items = computed(() => {
  return [...recurse(props.query)];
});
</script>
<template>
  <div v-if="items" class="query-results">
    <div v-for="(item, index) in items" :key="index" class="query-item">
      {{ item }}
    </div>
  </div>
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
