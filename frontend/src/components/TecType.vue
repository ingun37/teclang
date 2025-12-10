<script lang="ts" setup>
import type { TecType } from "@/schema/TecAstSchema.ts";
import { decodeUnknownSync } from "effect/Schema";
import { RefinedTecType } from "@/schema/TecRefined.ts";
import { iterateTec, queryDB } from "@/schema/IterateTec.ts";
import { useAppStore } from "@/stores/app.ts";

const props = defineProps<{ tecType: TecType }>();
const store = useAppStore();

const matrix = computed(() => {
  const db = store.graphDB;
  return Array.from(iterateTec(props.tecType, 0))
    .map(queryDB(db))
    .flatMap((x) => (x ? [x] : []));
});
const refined = computed((): RefinedTecType | null => {
  try {
    return decodeUnknownSync(RefinedTecType)(props.tecType);
  } catch (e) {
    console.warn("Error decoding TecType:", e);
    return null;
  }
});
</script>

<template>
  <v-sheet v-if="matrix">
    MATRIX!! {{ matrix.length }}
    <v-sheet v-for="(item, index) in matrix" :key="index">
      {{ item.key }}
    </v-sheet>
  </v-sheet>
  <v-sheet v-else-if="refined">
    <v-sheet v-if="refined.typeName === 'Text'">
      <Text :text="refined.parameters[0].str" />
    </v-sheet>
    <v-sheet v-if="refined.typeName === 'Pantone'">
      <Pantone :item="refined" />
    </v-sheet>
    <v-sheet v-if="refined.typeName === 'Render'">
      <Render :item="refined" />
    </v-sheet>
  </v-sheet>

  <v-sheet v-if="tecType.typeName === 'Logo'">
    <Logo />
  </v-sheet>
  <v-sheet v-if="tecType.typeName === 'Code'">
    <Code />
  </v-sheet>
  <v-sheet v-if="tecType.typeName === 'Name'">
    <Name />
  </v-sheet>
  <v-sheet v-if="tecType.typeName === 'PageNumber'">
    <PageNumber />
  </v-sheet>
  <v-sheet v-if="tecType.typeName === 'HStack'">
    <HStack :items="tecType.parameters" />
  </v-sheet>
  <v-sheet v-if="tecType.typeName === 'VStack'">
    <VStack :items="tecType.parameters" />
  </v-sheet>
</template>

<style lang="sass" scoped></style>
