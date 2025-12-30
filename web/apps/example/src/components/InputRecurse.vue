<script setup lang="ts">
import * as C from "codec";
import type { Axis } from "@/Axis.ts";
import * as E from "effect";
import InputParam from "@/components/InputParam.vue";
const props = defineProps<{
  axis: Axis;
  paramIndex: number;
  tecClass: C.TecType.TecClass;
  tecEnums: readonly C.TecType.TecEnum[];
}>();
const iter = computed<C.TecType.TecEnum | null>(() => {
  return E.pipe(
    props.tecEnums,
    E.Array.findFirst(
      (tecEnum) =>
        tecEnum.tecTypeName === props.tecClass.parameterTypes[props.paramIndex],
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
      v-for="value in iter.values"
      :key="value"
      :axis="axis === 'x' ? 'y' : 'x'"
      :paramIndex="paramIndex + 1"
      :tecClass="tecClass"
      :tecEnums="tecEnums"
    />
  </div>
  <div v-else>
    <InputParam />
  </div>
</template>

<style scoped lang="sass"></style>
