<script setup lang="ts">
import * as C from "codec";
import InputRecurse from "@/components/InputRecurse.vue";
const props = defineProps<{
  tecClass: C.TecType.TecClass;
  tecEnums: readonly C.TecType.TecEnum[];
}>();
</script>

<template>
  <InputRecurse
    axis="x"
    :param-index="0"
    :tec-class="tecClass"
    :tec-enums="tecEnums"
  />
</template>

<style scoped lang="sass"></style>
