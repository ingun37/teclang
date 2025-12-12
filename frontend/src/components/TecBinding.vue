<script lang="ts" setup>
import { type TecBinding as TecBindingType } from "@/schema/TecAstSchema.ts";
import { pipe, Record } from "effect";

const props = defineProps<{ binding: TecBindingType }>();

const emit = defineEmits<{
  deleted: [TecBindingType];
  updated: [TecBindingType];
}>();
const kvs = computed(() => {
  return pipe(props.binding.varMap, Record.toEntries);
});
</script>

<template>
  <v-sheet class="d-flex flex-column ga-1">
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
