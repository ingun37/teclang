<script lang="ts" setup>
import type { Pantone } from "@/schema/TecRefined.ts";

interface Props {
  readonly item: Pantone;
}
const props = defineProps<Props>();
</script>

<template>
  <div class="pantone-container">
    <div class="color-square"></div>
    <div class="color-label">{{ item.parameters[0].str }}</div>
  </div>
</template>

<style lang="sass" scoped>
.pantone-container
  display: flex
  flex-direction: column
  align-items: center
  gap: 8px

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
