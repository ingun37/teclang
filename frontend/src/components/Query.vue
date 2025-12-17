<script lang="ts" setup>
import { useAppStore } from "@/stores/app.ts";
import { TecQuery, TecType } from "@/schema/TecAstSchema.ts";
import { iterateQuery } from "@/schema/IterateTec.ts";
import { Array, pipe } from "effect";
import Reorder from "@/components/Reorder.vue";
import { nonNull } from "@/nonnull.ts";

const props = defineProps<{ query: TecQuery }>();
const store = useAppStore();

const items = computed(() => {
  const db = store.graphDB;

  return [...iterateQuery(db, props.query)];
});

const emit = defineEmits<{
  updated: [TecQuery];
}>();
function collectOperands(q: TecQuery): Array.NonEmptyArray<TecType> {
  if (q.left.tag === "TecQuery") {
    return Array.append(collectOperands(q.left), q.right);
  } else {
    return Array.make(q.left, q.right);
  }
}
const operands = computed(() => collectOperands(props.query));

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
function swap(i: number, j: number) {
  let newOrder = [...operands.value];
  newOrder[i] = nonNull(operands.value[j]);
  newOrder[j] = nonNull(operands.value[i]);
  const left = newOrder[0]!;
  const right = newOrder[1]!;
  const init = TecQuery.make({ op: ":-", left, right });
  const newQ = pipe(
    newOrder.slice(2),
    Array.reduce(init, (q, t) =>
      TecQuery.make({ op: ":-", left: q, right: t }),
    ),
  );
  emit("updated", newQ);
}
const initialOrientation = ref(false);
const orientation = computed(() =>
  initialOrientation.value ? "row" : "column",
);
</script>
<template>
  <v-sheet class="d-flex flex-row">
    <Reorder
      :labels="operands.map((x) => x.typeName)"
      @reorder="(x, y) => swap(x, y)"
    ></Reorder>
    <v-switch
      v-model="initialOrientation"
      class="ml-4"
      color="primary"
      density="compact"
      hide-details
      label="orientation"
    ></v-switch>
  </v-sheet>

  <v-sheet v-if="Array.isNonEmptyArray(items)">
    <EntryMatrix :axis="orientation" :entries="items"></EntryMatrix>
  </v-sheet>
  <div v-else class="no-results">No results</div>
</template>

<style lang="sass" scoped>

.no-results
  color: #999
  font-style: italic
</style>
