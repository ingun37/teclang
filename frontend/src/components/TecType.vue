<script lang="ts" setup>
import type { TecType } from "@/schema/TecAstSchema.ts";
import { decodeUnknownSync } from "effect/Schema";
import { RefinedTecType } from "@/schema/TecRefined.ts";

const props = defineProps<{ tecType: TecType }>();
const refined = computed(() =>
  decodeUnknownSync(RefinedTecType)(props.tecType),
);
</script>

<template>
  <v-sheet v-if="refined.typeName === 'Text'">
    <Text :text="refined.parameters[0].str" />
  </v-sheet>
</template>

<style lang="sass" scoped></style>
