<script lang="ts" setup>
import type { TypedEntry } from "@/schema/IterateTec.ts";

const props = defineProps<{ tEntry: TypedEntry }>();
</script>

<template>
  <v-sheet v-if="tEntry.typeName === 'Pantone'">
    <Pantone :item="tEntry" />
  </v-sheet>
  <v-sheet v-else-if="tEntry.typeName === 'Render'">
    <v-img
      :src="`/render/${tEntry.entry.indexSet[0]}-${tEntry.entry.indexSet[1]}.png`"
      alt="Render image"
      max-width="500px"
      min-width="100px"
    />
  </v-sheet>
  <v-sheet v-else-if="tEntry.typeName === 'Schematic'">
    <v-img
      :src="`/schematic/${tEntry.entry.indexSet[0]}.png`"
      alt="Render image"
      max-width="500px"
      min-width="100px"
    />
  </v-sheet>
  <v-sheet v-else-if="tEntry.typeName === 'Fabric'">
    <v-card
      subtitle="Composition"
      text="100% recycled treebark"
      title="ECOALF 78.6"
    >
    </v-card>
  </v-sheet>
</template>

<style lang="sass" scoped></style>
