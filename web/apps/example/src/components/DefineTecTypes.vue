<script setup lang="ts">
import * as C from "codec";
const model = defineModel<C.TecType.UniqueTecType>({ required: true });
</script>

<template>
  <v-container fluid class="pa-0">
    <v-row>
      <v-col cols="12">
        <DefineTecEnums v-model="model.enums" />
      </v-col>
      <v-col cols="12">
        <DefineTecType v-model="model.tecType" />
      </v-col>
    </v-row>
  </v-container>
</template>

<style scoped lang="sass"></style>
