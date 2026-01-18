<script setup lang="ts">
import * as C from "codec";
import InputParam from "@/components/InputParam.vue";
type TN = C.TecNode.TecNode;

const model = defineModel<TN>({ required: true });
const props = defineProps<{
  tecIndexedClass: C.TecClass.TecClass;
  allEnums: C.TecEnum.TecEnumAST;
  enableIndexListEditing: boolean;
}>();

const items = computed(() => {
  return props.tecIndexedClass.tecSignature.indexTypeSet.map(
    (typeName) =>
      props.allEnums.tecEnums.find(
        (enums) => (enums.tecEnumName as string) === (typeName as string),
      )!.tecEnumValues,
  );
});
</script>
<template>
  <div class="d-flex flex-row ga-2">
    <div
      v-if="enableIndexListEditing"
      v-for="(_, j) in model.indexCombination"
      :key="j"
      style="width: 12rem"
    >
      <v-select
        v-model="model.indexCombination[j]!"
        :items="items[j]"
        density="compact"
        hide-details
      />
    </div>
    <InputParam
      v-for="(_, i) in model.tecNodeAttributes"
      v-model="model.tecNodeAttributes[i]!"
      :index-combo="model.indexCombination"
      :param-type="tecIndexedClass.tecSignature.attributeTypeSet[i]!"
      :all-enums="allEnums"
    />
  </div>
</template>

<style scoped lang="sass"></style>
