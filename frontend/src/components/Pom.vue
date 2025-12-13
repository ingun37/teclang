<script lang="ts" setup>
import { type Pom } from "@/schema/TecRefined.ts";
import { iterateIndexSetsDBTuple } from "@/schema/IterateTec.ts";
import { useAppStore } from "@/stores/app.ts";
import { Array, flow, Order as O, pipe } from "effect";
import { nonNull } from "@/nonnull.ts";
import { isNonEmptyArray } from "effect/Array";

import type { NodeAttributes } from "@/NodeAttributes.ts";

const props = defineProps<{ pom: Pom }>();
type Item = { X: string; Y: string; value: number };
const table = computed((): Item[][] => {
  const db = useAppStore().graphDB;
  const pom = props.pom;
  const entries = Array.fromIterable(
    iterateIndexSetsDBTuple(db, pom.typeName, pom.parameters),
  );
  type E = (typeof entries)[number];

  if (!isNonEmptyArray(entries)) throw new Error("entries are empty");

  const sizeOrder: O.Order<string> = (a, b) => {
    const l = ["XS", "S", "M", "L", "XL", "2XL"];
    const diff = l.indexOf(a) - l.indexOf(b);
    return diff === 0 ? 0 : diff < 0 ? -1 : 1;
  };
  return pipe(
    entries,
    Array.sortWith((x) => x.indexSet[0], sizeOrder),
    Array.groupWith((x: E, y: E) => x.indexSet[0] === y.indexSet[0]),
    Array.map(
      flow(
        Array.sortWith((x) => x.indexSet[1], O.string),
        Array.map((x) => {
          const q: NodeAttributes = db.getNodeAttributes(x.node);
          const v = nonNull(q.meta.value);
          if (typeof v === "number")
            return { X: x.indexSet[0], Y: x.indexSet[1], value: v };
          else throw new Error("value is not a number");
        }),
      ),
    ),
  );
});
</script>

<template>
  <v-table>
    <thead>
      <tr>
        <th class="text-left"></th>
        <th v-for="(e, index) in table[0]" :key="index" class="text-left">
          {{ e.Y }}
        </th>
      </tr>
    </thead>

    <tbody>
      <tr v-for="(row, rowIndex) in table" :key="rowIndex">
        <td>{{ row[0].X }}</td>
        <td v-for="(item, cellIndex) in row" :key="cellIndex">
          {{ item.value }}
        </td>
      </tr>
    </tbody>
  </v-table>
</template>

<style lang="sass" scoped></style>
