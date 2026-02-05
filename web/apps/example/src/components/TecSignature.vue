<script setup lang="ts">
import * as C from "codec";
import * as E from "effect";
const model = defineModel<C.TecClass.TecSignature>({ required: true });
const props = defineProps<{ tecEnumAst: C.TecEnum.TecEnumAST }>();

function addIndex() {
  model.value = C.TecClass.TecSignature.make({
    attributeTypeSet: model.value.attributeTypeSet,
    indexTypeSet: E.Array.append(
      model.value.indexTypeSet,
      C.TecClass.TecClassIndex.make(
        props.tecEnumAst.tecEnums[props.tecEnumAst.tecEnums.length - 1]
          ?.tecEnumName ?? "undefined",
      ),
    ),
  });
}

function addAtt() {
  model.value = C.TecClass.TecSignature.make({
    attributeTypeSet: E.Array.append(
      model.value.attributeTypeSet,
      C.TecClass.TecClassAttribute.make("String"),
    ),
    indexTypeSet: model.value.indexTypeSet,
  });
}
function removeAttributeType(i: number) {
  const attributeTypeSet = E.Array.remove(model.value.attributeTypeSet, i);
  if (!E.Array.isNonEmptyReadonlyArray(attributeTypeSet)) {
    throw new Error("Cannot remove attribute type from empty set");
  }
  model.value = C.TecClass.TecSignature.make({
    attributeTypeSet,
    indexTypeSet: model.value.indexTypeSet,
  });
}
function removeIndexType(i: number) {
  const indexTypeSet = E.Array.remove(model.value.indexTypeSet, i);
  if (!E.Array.isNonEmptyReadonlyArray(indexTypeSet)) {
    throw new Error("Cannot remove index type from empty set");
  }
  model.value = C.TecClass.TecSignature.make({
    attributeTypeSet: model.value.attributeTypeSet,
    indexTypeSet,
  });
}
</script>

<template>
  <div class="d-flex flex-column ga-1">
    <SelectAttributeType
      v-for="(_, i) in model.attributeTypeSet"
      :key="i"
      :tec-enum-ast="tecEnumAst"
      v-model="model.attributeTypeSet[i]!"
      @delete="removeAttributeType(i)"
    />
    <v-btn prepend-icon="mdi-plus" variant="plain" size="small" @click="addAtt">
      Add Attribute
    </v-btn>
    <div class="text-caption mb-2">Index set</div>

    <SelectIndexType
      v-for="(_, i) in model.indexTypeSet"
      :key="i"
      :tec-enum-ast="tecEnumAst"
      v-model="model.indexTypeSet[i]!"
      @delete="removeIndexType(i)"
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
