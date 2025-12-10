<script lang="ts" setup>
import type { Render } from "@/schema/TecRefined.ts";
import { TecSide } from "@/schema/TecEnum.ts";

interface Props {
  readonly item: Render;
}
const props = defineProps<Props>();
const imgName = computed(() => {
  const colorway = props.item.parameters[0].int;
  const side = TecSide[props.item.parameters[1]];
  return `http://localhost:3000/render/${colorway}-${side.toLowerCase()}.png`;
});
</script>

<template>
  <v-img
    v-if="imgName"
    :src="imgName"
    alt="Render image"
    max-width="500px"
    min-width="100px"
  />
</template>
<style lang="sass" scoped></style>
