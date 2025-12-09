<script lang="ts" setup>
import type { TecQuery, TecType } from "@/schema/TecAST.ts";
import { useAppStore } from "@/stores/app.ts";

const props = defineProps<{ query: TecQuery }>();
const store = useAppStore();
function* recurse(left: TecType, right: TecType | TecQuery) {
  const db = store.graphDB;
  if (left.index.tag === "IndexR") {
    const end = left.index.to?.value ?? 10;

    for (let i = left.index.from.value; i < end; i++) {
      if (left.index1) {
        throw new Error("Not implemented");
      } else {
        const id = `${left.typeName}-${i}`;
        if (!db.hasNode(id)) break;
        const neighbours = db.directedNeighbors(id);
        for (let neighbour of neighbours) {
          yield [id, neighbour];
        }
      }
    }
  }
}
const items = computed(() => {
  const leftExp = props.query.left;
  const rightExp = props.query.right;
  if (
    leftExp.tag === "TecType" &&
    (rightExp.tag === "TecType" || rightExp.tag === "TecQuery")
  ) {
    return [...recurse(leftExp, rightExp)].flat();
  }
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
