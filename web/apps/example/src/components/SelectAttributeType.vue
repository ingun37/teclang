<script setup lang="ts">
import * as C from "codec";
const props = defineProps<{ tecEnumAst: C.TecEnum.TecEnumAST }>();
const model = defineModel<C.TecClass.TecClassAttribute>({ required: true });
defineEmits<{
  delete: [];
}>();
const primitiveTypes = ["String", "Number", "Image", "Color"];
const enumTypes = props.tecEnumAst.tecEnums.map((e) => e.tecEnumName);
</script>

<template>
  <v-select
    v-model="model"
    :items="primitiveTypes.concat(enumTypes)"
    density="compact"
    hide-details
    variant="outlined"
    append-icon="mdi-delete"
    @click:append="$emit('delete')"
  />
</template>

<style scoped lang="sass"></style>
