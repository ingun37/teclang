<template>
  <v-container fluid>
    <v-row>
      <v-col cols="12">
        <div :class="['text-h2']">Stage 1. Enum</div>
      </v-col>
      <v-col cols="12">
        <StageEnum @update="tecEnumAst = $event" />
      </v-col>

      <v-col cols="12">
        <div :class="['text-h2']">Stage 2. Class</div>
      </v-col>
      <v-col v-if="tecEnumAst" cols="12">
        <StageClass :tec-enum-ast="tecEnumAst" @update="tecClassAst = $event" />
      </v-col>

      <v-col cols="12">
        <div :class="['text-h2']">Stage 2. Data Input</div>
      </v-col>
      <v-col v-if="tecSchema" cols="12">
        <DataInputStage :tec-schema="tecSchema" />
      </v-col>
    </v-row>
  </v-container>
</template>

<script lang="ts" setup>
import * as C from "codec";
import DataInputStage from "@/components/DataInputStage.vue";
import StageEnum from "@/components/StageEnum.vue";
import StageClass from "@/components/StageClass.vue";
const tecEnumAst = ref<C.TecEnum.TecEnumAST | null>(null);
const tecSchema = ref<C.TecType.TecSchema | null>(null);
const tecClassAst = ref<C.TecClass.TecClassAST | null>(null);
</script>
