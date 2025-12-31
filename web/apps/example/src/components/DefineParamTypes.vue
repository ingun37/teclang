<script setup lang="ts">
import DefineParamType from "@/components/DefineParamType.vue";
import * as C from "codec";

const model = defineModel<readonly C.TecType.TecEnumName[]>({ required: true });
const props = defineProps<{ tecEnums: readonly C.TecType.TecEnum[] }>();

function addParamType() {
  model.value = model.value.concat(
    props.tecEnums[props.tecEnums.length - 1]!.tecTypeName,
  );
}
</script>

<template>
  <div class="d-flex flex-column">
    <div class="text-caption mb-2">Parameter Types</div>
    <DefineParamType
      v-for="(_, i) in model"
      v-model="model[i]!"
      :key="i"
      :tecEnums="props.tecEnums"
    />

    <v-btn
      prepend-icon="mdi-plus"
      variant="plain"
      size="small"
      @click="addParamType"
    >
      Add Parameter Type
    </v-btn>
  </div>
</template>

<style scoped lang="sass"></style>
