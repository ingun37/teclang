<script lang="ts" setup>
import type { TecAST } from "@/schema/TecAstSchema.ts";
import { useAppStore } from "@/stores/app.ts";
import { iterateQuery, iterateTecType } from "@/schema/IterateTec.ts";
import { Array } from "effect";

const props = defineProps<{ asts: readonly TecAST[] }>();

const transposed = computed(() => {
  const db = useAppStore().graphDB;

  const twoD = props.asts.map((ast) => {
    if (ast.tag === "TecQuery") {
      return Array.fromIterable(iterateQuery(db, ast));
    } else if (ast.tag === "TecType") {
      return Array.fromIterable(iterateTecType(db, ast)).map((x) => [x]);
    } else throw new Error("Unknown AST tag");
  });

  const minL = Math.min(...twoD.map((x) => x.length));
  Array;
  return Array.range(0, minL - 1).map((i) => twoD.map((x) => x[i]).flat());
});
</script>

<template>
  <v-sheet class="d-flex flex-row">
    <v-sheet v-for="(row, index) in transposed" :key="index">
      <v-sheet class="d-flex flex-column">
        <v-sheet v-for="(entry, index) in row" :key="index">
          <Single v-if="entry" :t-entry="entry" class="pa-1" />
        </v-sheet>
      </v-sheet>
    </v-sheet>
  </v-sheet>
</template>

<style lang="sass" scoped></style>
