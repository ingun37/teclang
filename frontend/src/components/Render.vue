<script lang="ts" setup>
import type { TypedEntry } from "@/schema/IterateTec.ts";

defineProps<{
  item: TypedEntry;
}>();
</script>

<template>
  <v-card
    :subtitle="`colorway ${item.entry.indexSet[0]} ${item.entry.indexSet[1]}`"
  >
    <Resizable>
      <v-img
        :src="`/render/${item.entry.indexSet[0]}-${item.entry.indexSet[1]}.png`"
        alt="Render image"
        max-width="500px"
        min-width="100px"
      />
    </Resizable>
  </v-card>
</template>
<style lang="sass" scoped></style>
