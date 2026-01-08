<script setup lang="ts">
import * as C from "codec";
import InputParam from "@/components/InputParam.vue";
type TN = C.TecNode.TecNode;

const model = defineModel<TN>({ required: true });
defineProps<{
  tecIndexedClass: C.TecClass.TecClass;
  allEnums: C.TecEnum.TecEnumAST;
}>();
</script>
<template>
  <InputParam
    v-for="(_, i) in model.tecNodeAttributes"
    v-model="model.tecNodeAttributes[i]"
    :index-combo="model.indexCombination"
    :param-type="tecIndexedClass.tecSignature.attributeTypeSet[i]!"
    :all-enums="allEnums"
  />
</template>

<style scoped lang="sass"></style>
