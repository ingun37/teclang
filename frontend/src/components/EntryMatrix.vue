<script lang="ts" setup>
import { Array, Equivalence, Option, pipe } from "effect";
import {
  type TypedEntry,
  typedEntryEq,
  typedEntryOrder,
} from "@/schema/IterateTec.ts";

type NE<A> = Array.NonEmptyArray<A>;
type RNE<A> = Array.NonEmptyReadonlyArray<A>;

const props = defineProps<{
  entries: RNE<RNE<TypedEntry>>;
  axis: "column" | "row";
}>();

const otherAxis = computed(() => (props.axis === "column" ? "row" : "column"));
type Group = { head: TypedEntry; tails: Array<NE<TypedEntry>> };
const groupedByFirstEntry = computed(() => {
  const grouped: NE<Group> = pipe(
    props.entries,
    Array.sort(Array.getOrder(typedEntryOrder)),
    Array.groupWith(Equivalence.mapInput(Array.headNonEmpty)(typedEntryEq)),
    Array.map((xx) => {
      const head = xx[0][0];
      const tails = pipe(
        xx,
        Array.map(Array.tailNonEmpty),
        Array.flatMapNullable((xs) => {
          if (Array.isNonEmptyArray(xs)) return xs;
          else return null;
        }),
      );
      return { head, tails };
    }),
  );
  return grouped;
});

const isTable = computed(() => {
  const grouped = groupedByFirstEntry.value;
  const headers = grouped.map((x) => x.head);
  const tt = pipe(
    grouped,
    Array.map((x) => x.tails),
  );
  if (
    Array.tailNonEmpty(tt).some(
      (ttt) => ttt.length !== Array.headNonEmpty(tt).length,
    )
  )
    return null;
  const tails = pipe(
    tt,
    Array.map(
      Array.map((entries) => {
        if (entries.length === 1) return Option.some(entries[0]);
        else return Option.none();
      }),
    ),
    Array.map(Option.all),
    Option.all,
  );
  if (Option.isNone(tails)) return null;
  return { headers, tails: tails.value };
});
</script>

<template>
  <v-sheet v-if="isTable">
    <v-table>
      <tbody>
        <tr v-for="(tail, index) in isTable.tails" :key="index">
          <td>
            <Single :t-entry="isTable.headers[index]!" />
          </td>
          <td v-for="(entry, index2) in tail" :key="index2">
            <Single :t-entry="entry"></Single>
          </td>
        </tr>
      </tbody>
    </v-table>
  </v-sheet>
  <v-sheet v-else :class="`d-flex flex-${axis}`">
    <v-sheet v-for="entries2D in groupedByFirstEntry">
      <v-sheet :class="`d-flex flex-${otherAxis}`">
        <Single :t-entry="entries2D.head"></Single>
        <EntryMatrix
          v-if="Array.isNonEmptyArray(entries2D.tails)"
          :axis="otherAxis"
          :entries="entries2D.tails"
        />
      </v-sheet>
    </v-sheet>
  </v-sheet>
</template>

<style lang="sass" scoped></style>
