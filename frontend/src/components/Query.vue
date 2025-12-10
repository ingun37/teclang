<script lang="ts" setup>
import { useAppStore } from "@/stores/app.ts";
import { Array, Effect } from "effect";
import type { TheGraph } from "@/graphdb.ts";
import type { TecQuery, TecQueryA } from "@/schema/TecAstSchema.ts";
import { decodeGenericIndexSets } from "@/schema/TecRefined.ts";
import { type Entry, iterateIndexSetsDB } from "@/schema/IterateTec.ts";

const props = defineProps<{ query: TecQuery }>();
const store = useAppStore();

type NE<T> = Array.NonEmptyArray<T>;
type QueryEntry = { entry: Entry; typeName: string };
function* recurseA(
  db: TheGraph,
  query: TecQueryA,
): Generator<[QueryEntry, QueryEntry]> {
  const left = query.left;
  const right = query.right;

  const leftIndexSets = Effect.runSync(decodeGenericIndexSets(left.parameters));
  const leftDB = iterateIndexSetsDB(db, left.typeName, leftIndexSets);
  const rightIndexSets = Effect.runSync(
    decodeGenericIndexSets(right.parameters),
  );
  const rightDB = Array.fromIterable(
    iterateIndexSetsDB(db, right.typeName, rightIndexSets),
  );
  for (const leftEntry of leftDB) {
    const neighbors = db.undirectedNeighbors(leftEntry.node);
    const intersect = rightDB.filter((rightEntry) =>
      neighbors.includes(rightEntry.node),
    );
    for (const rightEntry of intersect) {
      const xy: [QueryEntry, QueryEntry] = [
        { typeName: left.typeName, entry: leftEntry },
        { typeName: right.typeName, entry: rightEntry },
      ];
      xy.sort((a, b) => a.entry.node.localeCompare(b.entry.node));
      yield xy;
    }
  }
}
function* recurse(query: TecQuery): Generator<NE<QueryEntry>> {
  const db = store.graphDB;
  if (query.op === ":-") {
    yield* recurseA(db, query);
  } else if (query.op === ":>") {
    for (const chain of recurse(query.left)) {
      const rightIndexSets = Effect.runSync(
        decodeGenericIndexSets(query.right.parameters),
      );
      const rightDB = Array.fromIterable(
        iterateIndexSetsDB(db, query.right.typeName, rightIndexSets),
      );

      const edgeNode = chain.map((x) => x.entry.node).join("->");
      const neighbors = db.directedNeighbors(edgeNode);
      const intersect = rightDB.filter((rEntry) =>
        neighbors.includes(rEntry.node),
      );
      for (const rightEntry of intersect) {
        yield [...chain, { entry: rightEntry, typeName: query.right.typeName }];
      }
    }
  }
}

const items = computed(() => {
  return [...recurse(props.query)];
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
        <Single :entry="queryEntry.entry" :typeName="queryEntry.typeName" />
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
