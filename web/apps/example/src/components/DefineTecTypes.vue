<script setup lang="ts">
import * as C from "codec";
const model = defineModel<C.TecType.TecSchema>({ required: true });
</script>

<template>
  <v-container fluid class="pa-0">
    <v-row>
      <v-col cols="12">
        <DefineTecEnums v-model="model.tecEnums" />
      </v-col>
      <v-col cols="12">
        <DefineTecType v-model="model.tecSum" :tecEnums="model.tecEnums" />
      </v-col>
    </v-row>
  </v-container>
</template>

<style scoped lang="sass"></style>
