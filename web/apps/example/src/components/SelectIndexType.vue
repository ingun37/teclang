<script setup lang="ts">
import { computed } from "vue";
import * as C from "codec";
const props = defineProps<{ tecEnumAst: C.TecEnum.TecEnumAST }>();
const model = defineModel<C.TecClass.TecClassIndex>({ required: true });
defineEmits<{
  delete: [];
}>();
const options = computed<string[]>(() => {
  return props.tecEnumAst.tecEnums.map((e) => e.tecEnumName);
});
</script>

<template>
  <v-select
    v-model="model"
    :items="options"
    density="compact"
    hide-details
    variant="outlined"
    class="mb-2"
    append-icon="mdi-delete"
    @click:append="$emit('delete')"
  />
</template>

<style scoped lang="sass"></style>
