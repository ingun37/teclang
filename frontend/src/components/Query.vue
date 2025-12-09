<script lang="ts" setup>
import type { TecQuery, TecType } from "@/schema/TecAST.ts";
import { useAppStore } from "@/stores/app.ts";
import type Graph from "graphology";
import { Array, HashSet } from "effect";
import type { EdgeAttributes, NodeAttributes } from "@/graphdb.ts";

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
    if (tt.index.name === "*") {
      yield* db.filterNodes((node) => node.startsWith(tt.typeName));
    } else {
      const id = `${tt.typeName}-${tt.index.name}`;
      if (!db.hasNode(id)) return;
      yield id;
    }
  }
}

type NE<T> = Array.NonEmptyArray<T>;
type RecurseResult = [
  string,
  NE<{ edge: EdgeAttributes; next: string; nodeAttributes: NodeAttributes }>,
];
function* recurse(query: TecQuery): Generator<RecurseResult> {
  const db = store.graphDB;
  const left = query.left;
  const right = query.right;
  if (left.tag === "TecType" && right.tag === "TecType") {
    for (const leftID of iterateIDs(db, left)) {
      const rightIDs = HashSet.fromIterable(iterateIDs(db, right));
      {
        const neighbors = HashSet.fromIterable(db.undirectedNeighbors(leftID));
        const intersection = HashSet.intersection(neighbors, rightIDs);
        for (const next of intersection) {
          const edge = db.getEdgeAttributes(leftID, next);
          const nodeAttributes = db.getNodeAttributes(next);
          yield [leftID, [{ edge, next, nodeAttributes }]];
        }
      }
    }
  } else if (left.tag === "TecQuery" && right.tag === "TecType") {
    for (const [head, tail] of recurse(left)) {
      const lastEdge = Array.lastNonEmpty(tail).edge;
      const neighbors = HashSet.fromIterable(lastEdge.nodes);
      const rightIDs = HashSet.fromIterable(iterateIDs(db, right));
      const intersection = HashSet.intersection(neighbors, rightIDs);
      for (const next of intersection) {
        const nodeAttributes = db.getNodeAttributes(next);

        yield [
          head,
          Array.append(tail, {
            edge: db.getEdgeAttributes(lastEdge.edgeNode, next),
            nodeAttributes,
            next,
          }),
        ];
      }
    }
  } else {
    throw new Error("Not implemented");
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
