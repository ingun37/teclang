<script setup lang="ts">
import * as C from "codec";
import * as E from "effect";
import TecNodeSet from "@/components/TecNodeSet.vue";
const model = defineModel<C.TecNode.TecNodeAST>({ required: true });

const props = defineProps<{
  tecEnums: C.TecEnum.TecEnumAST;
  tecClasses: C.TecClass.TecClassAST;
}>();
</script>

<template>
  <v-container>
    <v-row>
      <v-col cols="12" v-for="(_, i) in model.tecNodeSets">
        <TecNodeSet
          v-model="model.tecNodeSets[i]!"
          :tec-enums="tecEnums"
          :tec-classes="tecClasses"
        />
      </v-col>
    </v-row>
  </v-container>
</template>

<style scoped lang="sass"></style>
