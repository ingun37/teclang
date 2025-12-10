<script lang="ts" setup>
import type { Render } from "@/schema/TecRefined.ts";

defineProps<{
  renderItem: Render;
}>();
</script>

<template>
  <Many :indexSets="renderItem.parameters" :typeName="renderItem.typeName" />
</template>
<style lang="sass" scoped></style>
