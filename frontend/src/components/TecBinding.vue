<script lang="ts" setup>
import { TecAST as TecASTType, TecBinding as TecBindingType, TecStr, TecType } from "@/schema/TecAstSchema.ts";
import { pipe, Record } from "effect";
import TecAST from "@/components/TecAST.vue";

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
function onDeleteExpression() {
  emit(
    "updated",
    TecBindingType.make({
      varMap: props.binding.varMap,
      expression: TecStr.make({ str: "(empty)" }),
    }),
  );
}
function onDeleteVariable(varName: string) {
  emit(
    "updated",
    TecBindingType.make({
      varMap: pipe(props.binding.varMap, Record.remove(varName)),
      expression: props.binding.expression,
    }),
  );
}
function onVariableUpdate(varName: string, newItem: TecASTType) {
  emit(
    "updated",
    TecBindingType.make({
      varMap: pipe(props.binding.varMap, Record.replace(varName, newItem)),
      expression: props.binding.expression,
    }),
  );
}
function onExpressionUpdate(newItem: TecASTType) {
  emit(
    "updated",
    TecBindingType.make({
      varMap: props.binding.varMap,
      expression: newItem,
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
      <v-card :subtitle="`variable ${k}`" class="pa-1">
        <TecAST
          :ast="v"
          @deleted="() => onDeleteVariable(k)"
          @updated="(x) => onVariableUpdate(k, x)"
        />
      </v-card>
    </v-sheet>
    <v-card class="pa-1" subtitle="final expression">
      <TecAST
        :ast="binding.expression"
        @deleted="onDeleteExpression"
        @updated="(x) => onExpressionUpdate(x)"
      ></TecAST>
    </v-card>
  </v-sheet>
</template>

<style lang="sass" scoped></style>
