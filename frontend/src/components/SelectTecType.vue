<script lang="ts" setup>
import { useAppStore } from "@/stores/app.ts";
import { getAllNodesOfType, iterateTypeNames } from "@/schema/IterateTec.ts";
import Graph from "graphology";
import Sigma from "sigma";
import { createNodeBorderProgram } from "@sigma/node-border";
import { Array, Order, pipe } from "effect";
import { configureSigma } from "@/sigmaHelper.ts";
import { iterateClique } from "@/functions.ts";
import { type NodeAttributes, NodeAttributesOrder } from "@/graphdb.ts";
import { nodeAttributesToQuery } from "@/transformers.ts";
import type { TecQuery } from "@/schema/TecAstSchema.ts";
import TecLang from "@/components/TecLang.vue";

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
const graph = shallowRef(new Graph());
const renderer = shallowRef<Sigma | null>(null);
const queries = shallowRef<readonly TecQuery[]>([]);
onMounted(() => {
  if (!sigmaContainer.value) return;
  graph.value.clear();

  const db = store.graphDB;
  const typeNames = Array.fromIterable(iterateTypeNames(db));
  const unitW = width / typeNames.length;
  const size = 8;
  const defaultColor = "#ccc";
  typeNames.forEach((typeName, i) => {
    const entries = getAllNodesOfType(db, typeName);
    const unitH = height / entries.length;

    entries.forEach((entry, j) => {
      graph.value.addNode(entry.node, {
        x: unitW * i,
        y: -unitH * j,
        size,
        label: entry.attributes.ids.map((x) => x.toString()).join("-"),
        color: defaultColor,
      });
    });
  });
  db.forEachUndirectedEdge((e, _, source, target) =>
    graph.value.addUndirectedEdge(source, target),
  );

  if (renderer.value) {
    renderer.value.removeAllListeners();
  } else {
    const R = new Sigma(graph.value, sigmaContainer.value, {
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
    });
    renderer.value = R;
    R.setSetting("nodeReducer", (node, data) => {
      return data;
    });
  }

  if (renderer.value) {
    configureSigma(renderer.value as any, graph.value, defaultColor, (subG) => {
      const cliques = iterateClique(subG);

      if (Array.isNonEmptyArray(cliques)) {
        queries.value = pipe(
          cliques,
          Array.map(Array.map((x) => db.getNodeAttributes(x))),
          Array.map(Array.sort(NodeAttributesOrder)),
          Array.sort(
            Array.getOrder(
              Order.mapInput((x: NodeAttributes) => x.typeName)(Order.string),
            ),
          ),
          Array.groupWith(
            (xs, ys) =>
              xs.map((x) => x.typeName).join("-") ===
              ys.map((y) => y.typeName).join("-"),
          ),
          Array.map(nodeAttributesToQuery),
        );
      } else queries.value = [];
    });
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
