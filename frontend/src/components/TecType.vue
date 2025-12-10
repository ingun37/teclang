<script lang="ts" setup>
import type { TecType } from "@/schema/TecAstSchema.ts";
import { decodeUnknownSync } from "effect/Schema";
import { RefinedTecType } from "@/schema/TecRefined.ts";

const props = defineProps<{ tecType: TecType }>();
const refined = computed((): RefinedTecType | null => {
  try {
    return decodeUnknownSync(RefinedTecType)(props.tecType);
  } catch (e) {
    console.error("Error decoding TecType:", e);
    return null;
  }
});
</script>

<template>
  <v-sheet v-if="refined">
    <v-sheet v-if="refined.typeName === 'Text'">
      <Text :text="refined.parameters[0].str" />
    </v-sheet>
    <v-sheet v-if="refined.typeName === 'Pantone'">
      <Pantone :item="refined" /> />
    </v-sheet>
    <v-sheet v-if="refined.typeName === 'Render'">
      <Render :item="refined" />>
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
</template>

<style lang="sass" scoped></style>
