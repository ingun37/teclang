<script setup lang="ts">
import * as C from "codec";
import InputClass from "@/components/InputClass.vue";
defineProps<{ tecSchema: C.TecType.TecSchema }>();
</script>

<template>
  <v-container>
    <v-row>
      <v-col
        v-for="tecClass in tecSchema.tecSum.classes"
        :key="tecClass.className"
        cols="12"
      >
        <InputClass :tec-class="tecClass" :tec-enums="tecSchema.tecEnums" />
      </v-col>
    </v-row>
  </v-container>
</template>

<style scoped lang="sass"></style>
