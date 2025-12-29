<script setup lang="ts">
import * as C from "codec";
defineProps<{ tecClass: C.TecType.TecClass }>();
</script>

<template>{{ tecClass.className }}</template>

<style scoped lang="sass"></style>
