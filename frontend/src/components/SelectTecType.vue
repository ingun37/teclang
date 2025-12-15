<script lang="ts" setup>
import { useAppStore } from "@/stores/app.ts";
import { iterateTypeNames } from "@/schema/IterateTec.ts";
import Graph from "graphology";
import Sigma from "sigma";
import { createNodeBorderProgram } from "@sigma/node-border";
import { Array, pipe } from "effect";
import { configureSigma } from "@/sigmaHelper.ts";
import type { TecQuery } from "@/schema/TecAstSchema.ts";
import TecLang from "@/components/TecLang.vue";
import { gNodeEqById0, gNodeEqByTypeName, gNodeOrder } from "@/GNode.ts";
import type { VGraph, VNodeAttributes } from "@/VGraph.ts";
import { nodeAttributesToQuery } from "@/transformers.ts";

const store = useAppStore();
const width = 1000;
const height = 500;
const containerW = `${width}px`;
const containerH = `${height}px`;
const typeNames = computed(() => {
  const db = store.graphDB;
  return Array.fromIterable(iterateTypeNames(db));
});
const sigmaContainer = useTemplateRef("sigma-container");
const graph = shallowRef<VGraph>(new Graph());
const renderer = shallowRef<Sigma<VNodeAttributes> | null>(null);
const queries = shallowRef<readonly TecQuery[]>([]);
onMounted(() => {
  if (!sigmaContainer.value) return;
  graph.value.clear();

  const db = store.graphDB;
  const entries = Array.fromIterable(db.nodeEntries());
  if (!Array.isNonEmptyArray(entries)) throw new Error("no nodes in graphdb");
  const groupedByTypeName = pipe(
    entries,
    Array.sort(gNodeOrder),
    Array.groupWith(gNodeEqByTypeName),
  );

  const unitW = width / groupedByTypeName.length;
  let offsetX = 0;
  const size = 8;
  const defaultColor = "#ccc";
  groupedByTypeName.forEach((entries) => {
    const groupedById0 = pipe(entries, Array.groupWith(gNodeEqById0));

    const hSize = groupedById0[0].length;
    const thisW = unitW * (1 + Math.log(hSize));

    const unitW2 = thisW / hSize;
    const vSize = groupedById0.length;
    const unitH = height / vSize;

    groupedById0.forEach((entries, j) => {
      entries.forEach((entry, i2) => {
        graph.value.addNode(entry.node, {
          x: offsetX + unitW2 * i2 + unitW2 / 2,
          y: -unitH * j + (unitH * (vSize - 1)) / 2,
          size,
          type: undefined,
          borderColor: defaultColor,
          label: entry.attributes.ids.map((x) => x.toString()).join("-"),
          color: defaultColor,
          dbNodeAttributes: entry.attributes,
        });
      });
    });

    offsetX += thisW;
  });
  db.forEachUndirectedEdge((e, _, source, target) =>
    graph.value.addUndirectedEdge(source, target),
  );

  if (renderer.value) {
    renderer.value.removeAllListeners();
  } else {
    renderer.value = new Sigma(graph.value, sigmaContainer.value, {
      zIndex: true,
      nodeProgramClasses: {
        border: createNodeBorderProgram({
          borders: [
            {
              size: { attribute: "borderSize", defaultValue: 0.3 },
              color: { attribute: "borderColor" },
            },
            { size: { fill: true }, color: { attribute: "color" } },
          ],
        }),
      },
      enableEdgeEvents: true,
    });
  }

  if (renderer.value) {
    configureSigma(
      renderer.value as any,
      graph.value,
      defaultColor,
      (vstate) => {
        if (vstate.categories)
          queries.value = pipe(
            vstate.categories.nodeCategories,
            Array.map(Array.map(Array.map((x) => x.attributes))),
            Array.map(nodeAttributesToQuery),
          );
      },
    );
  }
});
</script>

<template>
  <v-sheet>
    <v-container>
      <v-row>
        <v-col v-for="typeName in typeNames">
          <v-checkbox :label="typeName" density="compact" />
        </v-col>
      </v-row>
      <v-row>
        <v-col cols="12">
          <v-sheet :height="containerH" :width="containerW">
            <div ref="sigma-container" class="sigma-container"></div>
          </v-sheet>
        </v-col>
      </v-row>
      <v-row>
        <v-col v-for="ast in queries" cols="12">
          <TecLang :tec-ast="ast" />
          <Query :query="ast" />
        </v-col>
      </v-row>
    </v-container>
  </v-sheet>
</template>

<style lang="sass" scoped>
.sigma-container
  width: 100%
  height: 100%
  border: 2px solid #ccc
</style>
