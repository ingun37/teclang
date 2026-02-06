<script setup lang="ts">
import * as C from "codec";
import { createRandomAttributeValue } from "@/util.ts";

type TN = C.TecNode.TecNode;

const model = defineModel<TN | null>({ required: true });
const props = defineProps<{
  tecClass: C.TecClass.TecClass;
  enumAst: C.TecEnum.TecEnumAST;
  comb: C.TecNode.IndexCombination;
}>();
function create() {
  model.value = C.TecNode.TecNode.make({
    tecNodeAttributes: props.tecClass.tecSignature.attributeTypeSet.map(
      createRandomAttributeValue(props.tecClass, props.enumAst, props.comb),
    ),
    indexCombination: props.comb,
  });
}
</script>

<template>
  <v-btn v-if="model === null" @click="create">create</v-btn>
  <TecNode
    v-else
    v-model="model"
    :tec-indexed-class="tecClass"
    :all-enums="enumAst"
    :enable-index-list-editing="false"
  />
</template>

<style scoped lang="sass"></style>
