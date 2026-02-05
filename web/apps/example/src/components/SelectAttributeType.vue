<script setup lang="ts">
import { computed } from "vue";
import * as C from "codec";
const props = defineProps<{ tecEnumAst: C.TecEnum.TecEnumAST }>();
const model = defineModel<C.TecClass.TecClassAttribute>({ required: true });
defineEmits<{
  delete: [];
}>();
const primitiveTypes = ["String", "Number", "Image", "Color"];
const enumTypes = computed(() =>
  props.tecEnumAst.tecEnums.map((e) => e.tecEnumName),
);
const options = computed(() => primitiveTypes.concat(enumTypes.value));
const rules = [
  (v: string) => options.value.includes(v) || `Enum ${v} not exist`,
];
</script>

<template>
  <v-select
    v-model="model"
    :items="options"
    :rules="rules"
    density="compact"
    variant="outlined"
    append-icon="mdi-delete"
    @click:append="$emit('delete')"
  />
</template>

<style scoped lang="sass"></style>
