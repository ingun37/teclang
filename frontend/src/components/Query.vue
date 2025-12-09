<script lang="ts" setup>
import type { TecQuery, TecQueryA, TecType } from "@/schema/TecAST.ts";
import { useAppStore } from "@/stores/app.ts";
import { Array } from "effect";
import type { NodeAttributes, TheGraph } from "@/graphdb.ts";

const props = defineProps<{ query: TecQuery }>();
const store = useAppStore();

function* iterateIDs(db: TheGraph, tt: TecType) {
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
    if (tt.index.name === "*") {
      yield* db.filterNodes((node, attributes: NodeAttributes) =>
        attributes._tag === "TypeNode"
          ? attributes.typeName === tt.typeName
          : false,
      );
    } else {
      const id = `${tt.typeName}-${tt.index.name}`;
      if (!db.hasNode(id)) return;
      yield id;
    }
  }
}

type NE<T> = Array.NonEmptyArray<T>;
function* recurseA(
  db: TheGraph,
  query: TecQueryA,
): Generator<[string, string]> {
  const left = query.left;
  const right = query.right;
  const rightIDs = Array.fromIterable(iterateIDs(db, right));

  for (const leftID of iterateIDs(db, left)) {
    const neighbors = db.undirectedNeighbors(leftID);
    const intersect = rightIDs.filter((neighbor) =>
      neighbors.includes(neighbor),
    );
    for (const rightID of intersect) {
      yield [leftID, rightID];
    }
  }
}
function* recurse(query: TecQuery): Generator<NE<string>> {
  const db = store.graphDB;
  if (query.op === ":-") {
    yield* recurseA(db, query);
  } else if (query.op === ":>") {
    for (const chain of recurse(query.left)) {
      const rightIDs = Array.fromIterable(iterateIDs(db, query.right));
      const edgeNode = chain.join("->");
      const neighbors = db.directedNeighbors(edgeNode);
      const intersect = rightIDs.filter((neighbor) =>
        neighbors.includes(neighbor),
      );
      for (const rightID of intersect) {
        yield [...chain, rightID];
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
