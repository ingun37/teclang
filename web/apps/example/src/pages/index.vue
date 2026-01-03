<template>
  <v-container fluid>
    <v-row>
      <v-col cols="12">
        <div :class="['text-h2']">Stage 1. Enum</div>
      </v-col>
      <v-col cols="12">
        <StageEnum @update="tecEnumAST = $event" />
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
import SchemaStage from "@/components/SchemaStage.vue";
import DataInputStage from "@/components/DataInputStage.vue";
import StageEnum from "@/components/StageEnum.vue";
const tecEnumAST = ref<C.TecEnum.TecEnumAST | null>(null);
const tecSchema = ref<C.TecType.TecSchema | null>(null);
</script>
