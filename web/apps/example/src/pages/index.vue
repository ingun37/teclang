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
        <div :class="['text-h2']">Stage 3. Data Input</div>
      </v-col>
      <v-col v-if="tecEnumAst && tecClassAst" cols="12">
        <StageInput
          :tec-class-ast="tecClassAst"
          :tec-enum-ast="tecEnumAst"
          @update="tecNodeAst = $event"
        />
      </v-col>
      <v-col cols="12">
        <div :class="['text-h2']">(Optional) Stage 4. Sql</div>
      </v-col>
      <v-col v-if="tecEnumAst && tecClassAst && tecNodeAst">
        <StageSql
          :tec-class-ast="tecClassAst"
          :tec-enum-ast="tecEnumAst"
          :tec-node-ast="tecNodeAst"
        />
      </v-col>
    </v-row>
  </v-container>
</template>

<script lang="ts" setup>
import * as C from "codec";
import StageInput from "@/components/StageInput.vue";
import StageEnum from "@/components/StageEnum.vue";
import StageClass from "@/components/StageClass.vue";
const tecEnumAst = ref<C.TecEnum.TecEnumAST | null>(null);
const tecClassAst = ref<C.TecClass.TecClassAST | null>(null);
const tecNodeAst = ref<C.TecNode.TecNodeAST | null>(null);
</script>
