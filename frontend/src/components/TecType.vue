<script lang="ts" setup>
import type { TecType } from "@/schema/TecAstSchema.ts";
import { decodeUnknownSync } from "effect/Schema";
import { RefinedTecType } from "@/schema/TecRefined.ts";
import Zip from "@/components/Zip.vue";

const props = defineProps<{ tecType: TecType }>();

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
  <v-sheet v-if="refined">
    <v-sheet v-if="refined.typeName === 'Text'">
      <Text :text="refined.parameters[0]!.str" />
    </v-sheet>
    <v-sheet v-else-if="refined.typeName === 'Pantone'">
      <Many :index-sets="refined.parameters" :type-name="refined.typeName" />
    </v-sheet>
    <v-sheet v-else-if="refined.typeName === 'Fabric'">
      <Fabrics :item="refined" />
    </v-sheet>
    <v-sheet v-else-if="refined.typeName === 'Render'">
      <Renders :render-item="refined" />
    </v-sheet>
    <v-sheet v-else-if="refined.typeName === 'Schematic'">
      <Many :index-sets="refined.parameters" :type-name="refined.typeName" />
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
  <v-sheet v-if="tecType.typeName === 'Zip'">
    <Zip :asts="tecType.parameters" />
  </v-sheet>
</template>

<style lang="sass" scoped></style>
