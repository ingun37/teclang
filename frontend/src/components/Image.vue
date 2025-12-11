<script lang="ts" setup>
import Resizable from "@/components/Resizable.vue";

const props = defineProps<{ name: string }>();
const src = computed(() => `/imgs/${props.name}`);
</script>

<template>
  <Resizable>
    <v-card class="pa-1">
      <v-img :src="src" />
    </v-card>
  </Resizable>
</template>

<style lang="sass" scoped></style>
