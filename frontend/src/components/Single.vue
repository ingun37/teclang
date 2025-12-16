<script lang="ts" setup>
import type { TypedEntry } from "@/schema/IterateTec.ts";

const props = defineProps<{ tEntry: TypedEntry }>();
</script>

<template>
  <v-sheet v-if="tEntry.typeName === 'Pantone'">
    <Pantone :item="tEntry" />
  </v-sheet>
  <v-sheet v-else-if="tEntry.typeName === 'Render'">
    <Render :item="tEntry" />
  </v-sheet>
  <v-sheet v-else-if="tEntry.typeName === 'Schematic'">
    <Resizable>
      <v-img
        :src="`/schematic/${tEntry.entry.indexSet[0]}.png`"
        alt="Render image"
        max-width="500px"
        min-width="100px"
      />
    </Resizable>
  </v-sheet>
  <v-sheet v-else-if="tEntry.typeName === 'Fabric'">
    <v-card
      subtitle="Composition"
      text="100% recycled treebark"
      title="ECOALF 78.6"
    >
    </v-card>
  </v-sheet>
  <v-sheet v-else-if="tEntry.typeName === 'Pom'">
    {{ tEntry.entry.attributes.meta.value }}
  </v-sheet>
  <v-sheet v-else>
    {{ tEntry.typeName }} {{ tEntry.entry.indexSet.join(",") }}
  </v-sheet>
</template>

<style lang="sass" scoped></style>
