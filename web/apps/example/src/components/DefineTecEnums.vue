<script setup lang="ts">
import * as C from "codec";

const model = defineModel<readonly C.TecType.TecEnum[]>({ required: true });
</script>

<template>
  <v-card>
    <v-card-subtitle>Enums</v-card-subtitle>
    <v-card-text>
      <v-container>
        <v-row dense>
          <v-col cols="auto" v-for="(_, i) in model" :key="i">
            <DefineTecEnum v-model="model[i]!" />
          </v-col>
        </v-row>
      </v-container>
    </v-card-text>
  </v-card>
</template>

<style scoped lang="sass"></style>
