<script lang="ts" setup>
const props = defineProps<{ name: string }>();
const src = computed(() => `/imgs/${props.name}`);
</script>

<template>
  <v-card class="pa-1">
    <v-img :src="src" />
  </v-card>
</template>

<style lang="sass" scoped></style>
