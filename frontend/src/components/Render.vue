<script lang="ts" setup>
import type { TecType } from "@/schema/TecAST.ts";

interface Props {
  readonly item: TecType;
}
const props = defineProps<Props>();
const imgName = computed(() => {
  if (props.item.index.tag === "IndexE") {
    if (props.item.index1)
      if (props.item.index1.tag === "IndexE") {
        return `http://localhost:3000/render/${props.item.index.contents.value}-${props.item.index1.contents.label.toLowerCase()}.png`;
      }
  }
});
</script>

<template>
  <v-img v-if="imgName" :src="imgName" alt="Render image" />
</template>
<style lang="sass" scoped></style>
