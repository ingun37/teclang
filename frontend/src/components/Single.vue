<script lang="ts" setup>
import type { Entry } from "@/schema/IterateTec.ts";

const props = defineProps<{ typeName: string; entry: Entry }>();
</script>

<template>
  <v-sheet v-if="typeName === 'Pantone'">
    <v-card class="pantone-container" subtitle="PANTONE 2001">
      <v-card-text>
        <div class="color-square"></div>
        <div class="color-label">#76ab2a</div>
      </v-card-text>
    </v-card>
  </v-sheet>
  <v-sheet v-else-if="typeName === 'Render'">
    <v-img
      :src="`/render/${entry.indexSet[0]}-${entry.indexSet[1]}.png`"
      alt="Render image"
      max-width="500px"
      min-width="100px"
    />
  </v-sheet>
  <v-sheet v-else-if="typeName === 'Fabric'">
    <v-card
      subtitle="Composition"
      text="100% recycled treebark"
      title="ECOALF 78.6"
    >
    </v-card>
  </v-sheet>
</template>

<style lang="sass" scoped>
.pantone-container
  display: flex
  flex-direction: column
  align-items: center

.color-square
  width: 60px
  height: 60px
  background-color: #40E0D0
  border: 1px solid #ccc
  border-radius: 4px

.color-label
  font-size: 14px
  text-align: center
</style>
