<script lang="ts" setup>
import type { TypedEntry } from "@/schema/IterateTec.ts";
import { useAppStore } from "@/stores/app.ts";
import type { NodeAttributes } from "@/graphdb.ts";

const props = defineProps<{
  readonly item: TypedEntry;
}>();

const color = computed((): string => {
  const db = useAppStore().graphDB;
  const att: NodeAttributes = db.getNodeAttributes(props.item.entry.node);
  return att.meta.color;
});
</script>

<template>
  <v-card class="pantone-container" subtitle="PANTONE 2001">
    <v-card-text>
      <div :style="{ backgroundColor: color }" class="color-square"></div>
      <div class="color-label">{{ color }}</div>
    </v-card-text>
  </v-card>
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
