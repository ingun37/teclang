<script lang="ts" setup>
import {
  TecBinding as TecBindingType,
  TecStr,
  TecType,
} from "@/schema/TecAstSchema.ts";
import { pipe, Record } from "effect";

const props = defineProps<{ binding: TecBindingType }>();

const emit = defineEmits<{
  deleted: [TecBindingType];
  updated: [TecBindingType];
}>();
const kvs = computed(() => {
  return pipe(props.binding.varMap, Record.toEntries);
});
function onHStack() {
  const e = props.binding.expression;
  let parameters = e.tag === "TecType" ? [...e.parameters] : [];
  if (parameters.length === 0) parameters.push(TecStr.make({ str: "(empty)" }));
  emit(
    "updated",
    TecBindingType.make({
      varMap: props.binding.varMap,
      expression: TecType.make({
        typeName: "HStack",
        parameters,
      }),
    }),
  );
}
</script>

<template>
  <v-sheet class="d-flex flex-column ga-1">
    <v-sheet class="d-flex flex-row ga-1">
      <v-btn @click="onHStack">HStack</v-btn>
    </v-sheet>
    <v-sheet v-for="[k, v] in kvs">
      <v-card :subtitle="`variable ${k}`">
        <TecAST :ast="v" />
      </v-card>
    </v-sheet>
    <v-card subtitle="final expression">
      <TecAST :ast="binding.expression"></TecAST>
    </v-card>
  </v-sheet>
</template>

<style lang="sass" scoped></style>
