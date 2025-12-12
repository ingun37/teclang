<script lang="ts" setup>
import { useAppStore } from "@/stores/app.ts";
import { iterateIndexTuples, iterateTypeNames } from "@/schema/IterateTec.ts";
import type { IndexItem } from "@/schema/TecRefined.ts";

const store = useAppStore();

const typeNames = computed(() => {
  const db = store.graphDB;
  return Array.from(iterateTypeNames(db));
});
const typeName = ref("");
const allEntries = ref<IndexItem[][]>([]);
watch(typeName, () => {
  if (typeName.value === "") return (allEntries.value = []);
  else {
    const db = store.graphDB;
    allEntries.value = Array.from(iterateIndexTuples(db, typeName.value));
  }
});

const entryDegree = computed(() => {
  return allEntries.value[0]?.length ?? 0;
});
</script>

<template>
  <v-menu location="bottom">
    <template v-slot:activator="{ props }">
      <v-btn v-bind="props" variant="outlined">
        {{ typeName || "Select Type" }}
      </v-btn>
    </template>
    <v-list>
      <v-list-item
        v-for="name in typeNames"
        :key="name"
        @click="typeName = name"
      >
        <v-list-item-title>{{ name }}</v-list-item-title>
      </v-list-item>
    </v-list>
  </v-menu>
  <v-sheet class="d-flex flex-row">
    <v-sheet v-for="i in entryDegree"> degree{{ i }} </v-sheet>
  </v-sheet>
</template>

<style lang="sass" scoped></style>
