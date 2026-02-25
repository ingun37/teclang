<script setup lang="ts">
import * as C from "codec";
import TecPnumTable from "@/components/TecPnumTable.vue";

const model = defineModel<C.TecPnum.TecPnum>({ required: true });
</script>

<template>
  <v-card>
    <v-card-title>{{ model._tecPnumName }}</v-card-title>
    <v-card-text>
      <TecPnumTable v-if="model._tecPnumTable" v-model="model._tecPnumTable" />
    </v-card-text>
  </v-card>
</template>

<style scoped lang="sass"></style>
