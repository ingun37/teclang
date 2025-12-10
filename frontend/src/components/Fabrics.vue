<script lang="ts" setup>
import type { Fabric } from "@/schema/TecRefined.ts";

defineProps<{
  readonly item: Fabric;
}>();
</script>
<template>
  <Many :indexSets="item.parameters" :typeName="item.typeName" />
</template>

<style lang="sass" scoped></style>
