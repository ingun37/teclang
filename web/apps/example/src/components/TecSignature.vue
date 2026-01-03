<script setup lang="ts">
import * as C from "codec";
import * as E from "effect";
const model = defineModel<C.TecClass.TecSignature>({ required: true });
const props = defineProps<{ tecEnumAst: C.TecEnum.TecEnumAST }>();

function addIndex() {
  model.value = C.TecClass.TecSignature.make({
    attributes: model.value.attributes,
    indexSet: E.Array.append(
      model.value.indexSet,
      C.TecClass.TecClassIndex.make(
        props.tecEnumAst.tecEnums[props.tecEnumAst.tecEnums.length - 1]
          ?.tecEnumName ?? "undefined",
      ),
    ),
  });
}
const options = computed<string[]>(() => {
  return props.tecEnumAst.tecEnums.map((e) => e.tecEnumName);
});
</script>

<template>
  <div class="d-flex flex-column ga-1">
    <v-select
      v-model="model.attributes[0]"
      :items="['String', 'Number', 'Image']"
      density="compact"
      hide-details
      variant="outlined"
    />

    <div class="text-caption mb-2">Index set</div>

    <v-select
      v-for="(_, i) in model.indexSet"
      v-model="model.indexSet[i]!"
      :key="i"
      :items="options"
      density="compact"
      hide-details
      variant="outlined"
      class="mb-2"
    />

    <v-btn
      prepend-icon="mdi-plus"
      variant="plain"
      size="small"
      @click="addIndex"
    >
      Add Index
    </v-btn>
  </div>
</template>

<style scoped lang="sass"></style>
