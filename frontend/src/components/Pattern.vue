<script lang="ts" setup>
import type { TypedEntry } from "@/schema/IterateTec.ts";

defineProps<{
  item: TypedEntry;
}>();
</script>

<template>
  <v-card :subtitle="`pattern ${item.entry.indexSet[0]}`">
    <Resizable>
      <v-img
        :src="`/pattern/${item.entry.indexSet[0]}.png`"
        alt="Pattern image"
        min-width="100px"
      />
    </Resizable>
  </v-card>
</template>
<style lang="sass" scoped></style>
