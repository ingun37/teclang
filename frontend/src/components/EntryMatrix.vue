<script lang="ts" setup>
import { Array, Equivalence, pipe } from "effect";
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
</script>

<template>
  <v-sheet :class="`d-flex flex-${axis}`">
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
