<script lang="ts" setup>
import type { Pantone } from "@/schema/TecRefined.ts";

defineProps<{
  readonly item: Pantone;
}>();
</script>

<template>
  <Many :indexSets="item.parameters" :typeName="item.typeName" />
</template>

<style lang="sass" scoped></style>
