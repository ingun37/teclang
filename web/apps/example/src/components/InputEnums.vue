<script setup lang="ts">
import * as C from "codec";
import * as E from "effect";
import InputEnum from "@/components/InputEnum.vue";
const props = defineProps<{
  tecEnums: C.TecEnum.TecEnumAST;
}>();
const enumsWithRep = computed(() =>
  props.tecEnums.tecEnums.filter((e) =>
    E.Array.isNonEmptyReadonlyArray(e.tecEnumRepresentation.repAttribs),
  ),
);
</script>

<template>
  <div class="d-flex flex-row">
    <InputEnum
      v-for="e in enumsWithRep"
      :key="e.tecEnumName"
      :tec-enum="e"
      :all-enums="props.tecEnums"
    />
  </div>
</template>

<style scoped lang="sass"></style>
