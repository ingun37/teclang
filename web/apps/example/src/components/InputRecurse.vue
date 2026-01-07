<script setup lang="ts">
import * as C from "codec";
import type { Axis } from "@/Axis.ts";
import * as E from "effect";
import InputParam from "@/components/InputParam.vue";
const props = defineProps<{
  indexCombo: C.TecEnum.TecEnumValue[];
  axis: Axis;
  paramIndex: number;
  tecIndexedClass: C.TecClass.TecClass;
  tecEnums: C.TecEnum.TecEnumAST;
}>();
defineEmits<{
  (
    e: "update",
    indexCombo: C.TecEnum.TecEnumValue[],
    paramTypeIndex: number,
    value: any,
  ): void;
}>();
const iter = computed<C.TecEnum.TecEnum | null>(() => {
  return E.pipe(
    props.tecEnums.tecEnums,
    E.Array.findFirst(
      (tecEnum) =>
        (tecEnum.tecEnumName as string) ===
        (props.tecIndexedClass.tecSignature.indexTypeSet[
          props.paramIndex
        ] as string),
    ),
    E.Option.getOrNull,
  );
});
</script>

<template>
  <div
    v-if="iter !== null"
    :class="['d-flex', axis === 'x' ? 'flex-row' : 'flex-column']"
  >
    <InputRecurse
      v-for="value in iter.tecEnumValues"
      :key="value"
      :index-combo="[...indexCombo, value]"
      :axis="axis === 'x' ? 'y' : 'x'"
      :paramIndex="paramIndex + 1"
      :tecIndexedClass="tecIndexedClass"
      :tecEnums="tecEnums"
      @update="(x, y, z) => $emit('update', x, y, z)"
    />
  </div>
  <div v-else class="d-flex flex-column ga-1">
    <InputParam
      v-for="(_, i) in tecIndexedClass.tecSignature.attributeTypeSet"
      :index-combo="indexCombo"
      :param-type="tecIndexedClass.tecSignature.attributeTypeSet[i]!"
      :all-enums="tecEnums"
      @update="$emit('update', indexCombo, i, $event)"
    />
  </div>
</template>

<style scoped lang="sass"></style>
