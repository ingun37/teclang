<script lang="ts" setup>
import type { Fabric } from "@/schema/TecRefined.ts";
import { useAppStore } from "@/stores/app.ts";
import { iterateIndexSetsDB } from "@/schema/IterateTec.ts";

interface Props {
  readonly item: Fabric;
}
const props = defineProps<Props>();

const matrix = computed(() => {
  const db = useAppStore().graphDB;
  const renderItem: Fabric = props.item;
  return Array.from(
    iterateIndexSetsDB(db, renderItem.typeName, renderItem.parameters),
  );
});
</script>
<template>
  <div>
    <v-card v-for="(item, idx) in matrix" :key="idx" class="my-4">
      <div class="text-subtitle-1 pa-2">ECOALF 78.6</div>
      <div class="text-caption pa-2 border-t">
        <div class="opacity-70">Class</div>
        <div class="font-weight-medium">
          Composition: 100% recycled treebark
        </div>
      </div>
    </v-card>
  </div>
</template>

<style lang="sass" scoped></style>
