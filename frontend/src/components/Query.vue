<script lang="ts" setup>
import { useAppStore } from "@/stores/app.ts";
import { Array } from "effect";
import type { TheGraph } from "@/graphdb.ts";
import { compareIndex } from "@/CompareIndex.ts";
import type { TecQuery, TecQueryA, TecType } from "@/schema/TecAstSchema.ts";
import { iterateDB, type IterItem } from "@/schema/IterateTec.ts";

const props = defineProps<{ query: TecQuery }>();
const store = useAppStore();

function* iterateIDs(db: TheGraph, tt: TecType): Generator<string> {
  const nodes = db.filterNodes((node, att) => {
    if (att._tag !== "TypeNode") return false;
    if (att.typeName !== tt.typeName) return false;
    if (att.index) {
      if (!tt.parameters[0]) return false;
      if (compareIndex(att.index, tt.parameters[0])) {
        if (att.index1) {
          if (!tt.parameters[1]) return false;
          return compareIndex(att.index1, tt.parameters[1]);
        } else {
          return !tt.parameters[1];
        }
      } else return false;
    }
    return false;
  });
  yield* nodes;
}

type NE<T> = Array.NonEmptyArray<T>;
function* recurseA(
  db: TheGraph,
  query: TecQueryA,
): Generator<[IterItem, IterItem]> {
  const left = query.left;
  const right = query.right;
  const rightItems = iterateDB(db, right);

  for (const leftItem of iterateDB(db, left)) {
    const neighbors = db.undirectedNeighbors(leftItem.key);
    const intersect = rightItems.filter((rightItem) =>
      neighbors.includes(rightItem.key),
    );
    for (const rightItem of intersect) {
      const xy: [IterItem, IterItem] = [leftItem, rightItem];
      xy.sort((a, b) => a.key.localeCompare(b.key));
      yield xy;
    }
  }
}
function* recurse(query: TecQuery): Generator<NE<IterItem>> {
  const db = store.graphDB;
  if (query.op === ":-") {
    yield* recurseA(db, query);
  } else if (query.op === ":>") {
    for (const chain of recurse(query.left)) {
      const rightItems = iterateDB(db, query.right);
      const edgeNode = chain.map((x) => x.key).join("->");
      const neighbors = db.directedNeighbors(edgeNode);
      const intersect = rightItems.filter((rightItem) =>
        neighbors.includes(rightItem.key),
      );
      for (const rightItem of intersect) {
        yield [...chain, rightItem];
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
