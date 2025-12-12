<script lang="ts" setup>
import { useAppStore } from "@/stores/app.ts";
import { getAllNodesOfType, iterateTypeNames } from "@/schema/IterateTec.ts";
import Graph from "graphology";
import Sigma from "sigma";

const store = useAppStore();
const width = 1200;
const height = 600;
const containerW = `${width}px`;
const containerH = `${height}px`;
const typeNames = computed(() => {
  const db = store.graphDB;
  return Array.from(iterateTypeNames(db));
});
const sigmaContainer = useTemplateRef("sigma-container");
const graph = ref(new Graph());
const renderer = ref<Sigma | null>(null);
onMounted(() => {
  if (!sigmaContainer.value) return;
  graph.value.clear();

  const db = store.graphDB;
  const typeNames = Array.from(iterateTypeNames(db));
  const unitW = width / typeNames.length;
  const size = 8;
  typeNames.forEach((typeName, i) => {
    const entries = getAllNodesOfType(db, typeName);
    const unitH = height / entries.length;

    entries.forEach((entry, j) => {
      graph.value.addNode(entry.node, {
        x: unitW * i,
        y: -unitH * j,
        size,
        label: entry.attributes.ids.map((x) => x.toString()).join("-"),
      });
    });
  });
  db.forEachUndirectedEdge((e, _, source, target) =>
    graph.value.addEdge(source, target),
  );
  if (renderer.value) {
    renderer.value.refresh();
  } else {
    renderer.value = new Sigma(graph.value, sigmaContainer.value);
  }
});
</script>

<template>
  <v-sheet :height="containerH" :width="containerW">
    <v-container>
      <v-row>
        <v-col v-for="typeName in typeNames"> {{ typeName }} </v-col>
      </v-row>
    </v-container>
    <div ref="sigma-container" class="sigma-container"></div>
  </v-sheet>
</template>

<style lang="sass" scoped>
.sigma-container
  width: 100%
  height: 100%
  border: 2px solid #ccc
</style>
