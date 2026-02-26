<script setup lang="ts">
import * as C from "codec";
import TecPnum from "@/components/TecPnum.vue";
const model = defineModel<C.TecPnum.TecPnumAST>({ required: true });
</script>

<template>
  <v-container fluid>
    <v-row>
      <v-col cols="12" v-for="i in model.tecPnums.length">
        <TecPnum v-model="model.tecPnums[i - 1]!" :pnum-ast="model" />
      </v-col>
    </v-row>
  </v-container>
</template>

<style scoped lang="sass"></style>
