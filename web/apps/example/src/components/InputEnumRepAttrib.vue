<script setup lang="ts">
import * as C from "codec";

const props = defineProps<{
  repAttrib: C.TecEnum.TecEnumRepresentationAttribute;
  allEnums: C.TecEnum.TecEnumAST;
}>();
</script>

<template>
  <div class="d-flex flex-row ga-2">
    <InputParam
      :param-type="repAttrib"
      :all-enums="props.allEnums"
      :index-combo="[props.repAttrib]"
    />
  </div>
</template>

<style scoped lang="sass"></style>
