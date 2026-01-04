<script setup lang="ts">
import * as C from "codec";
import InputClass from "@/components/InputClass.vue";
defineProps<{
  tecClassAst: C.TecClass.TecClassAST;
  tecEnumAst: C.TecEnum.TecEnumAST;
}>();
</script>

<template>
  <v-container>
    <v-row>
      <v-col cols="12">
        <InputEnums :tec-enums="tecEnumAst" />
      </v-col>
      <v-col
        v-for="tecClass in tecClassAst.tecClasses"
        :key="tecClass.tecClassName"
        cols="12"
      >
        <InputClass :tec-class="tecClass" :tec-enums="tecEnumAst" />
      </v-col>
    </v-row>
  </v-container>
</template>

<style scoped lang="sass"></style>
