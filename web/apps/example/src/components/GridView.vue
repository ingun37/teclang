<script setup lang="ts">
import type { Axis } from "@/Axis.ts";
import * as C from "codec";
import * as E from "effect";
import GridView1D from "@/components/GridView1D.vue";
type RNE<T> = E.Array.NonEmptyReadonlyArray<T>;
type TN = C.TecNode.TecNode;

const model = defineModel<RNE<TN>>({ required: true });

const props = defineProps<{
  tecClass: C.TecClass.TecClass;
  enumAst: C.TecEnum.TecEnumAST;
}>();
</script>

<template>
  <GridView1D
    v-if="tecClass.tecSignature.indexTypeSet.length === 1"
    :tec-class="tecClass"
    :enum-ast="enumAst"
    v-model="model"
  />
  <GridView2D
    v-else-if="tecClass.tecSignature.indexTypeSet.length === 2"
    :tec-class="tecClass"
    :enum-ast="enumAst"
    v-model="model"
  />
  <GridView3D
    v-else-if="tecClass.tecSignature.indexTypeSet.length === 3"
    :tec-class="tecClass"
    :enum-ast="enumAst"
    v-model="model"
  />

  <div v-else>aoeuaouaoeuaeu</div>
</template>

<style scoped lang="sass"></style>
