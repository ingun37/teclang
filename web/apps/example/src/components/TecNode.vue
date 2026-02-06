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

const emit = defineEmits<{
  (e: "delete"): void;
}>();
</script>
<template>
  <div class="flex-grow-1 d-flex flex-row ga-2">
    <div
      v-if="enableIndexListEditing"
      v-for="(_, j) in model.indexCombination"
      :key="j"
      style="width: 12rem"
    >
      <TecNodeIndex
        v-model="model.indexCombination[j]!"
        :tec-indexed-class="tecIndexedClass"
        :tec-class-index="props.tecIndexedClass.tecSignature.indexTypeSet[j]!"
        :all-enums="allEnums"
        :enable-index-list-editing="enableIndexListEditing"
      ></TecNodeIndex>
    </div>
    <InputParam
      v-for="(_, i) in model.tecNodeAttributes"
      v-model="model.tecNodeAttributes[i]!"
      :index-combo="model.indexCombination"
      :param-type="tecIndexedClass.tecSignature.attributeTypeSet[i]!"
      :all-enums="allEnums"
      class="flex-grow-1"
    />
    <v-btn
      v-if="enableIndexListEditing"
      icon="mdi-delete"
      variant="text"
      density="compact"
      color="error"
      @click="emit('delete')"
      class="mt-1"
    ></v-btn>
  </div>
</template>

<style scoped lang="sass"></style>
