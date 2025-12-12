<script lang="ts" setup>
import { useAppStore } from "@/stores/app.ts";
import { getAllNodesOfType, iterateTypeNames } from "@/schema/IterateTec.ts";
import Graph from "graphology";
import Sigma from "sigma";
import { createNodeBorderProgram } from "@sigma/node-border";
import { Array } from "effect";
import { foldIntersect, foldUnion } from "@/functions.ts";

const store = useAppStore();
const width = 1200;
const height = 600;
const containerW = `${width}px`;
const containerH = `${height}px`;
const typeNames = computed(() => {
  const db = store.graphDB;
  return Array.fromIterable(iterateTypeNames(db));
});
const sigmaContainer = useTemplateRef("sigma-container");
const graph = ref(new Graph());
const renderer = ref<Sigma | null>(null);
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
      console.log("reducer is called");
      return data;
    });
  }

  if (renderer.value) {
    const R = renderer.value;
    const state = {
      RNode: "",
      R2Nodes: [] as string[],
      REdges: [] as string[],
      BNodes: [] as string[],
      BEdges: [] as string[],
      G2Nodes: [] as string[],
      GEdges: [] as string[],
    };
    const GColor = "#00ff00";
    const BColor = "#0000ff";
    const RColor = "#ff0000";
    function updateStyle() {
      const G = graph.value;
      G.updateEachNodeAttributes((_, att) => ({
        ...att,
        color: defaultColor,
        type: undefined,
        borderColor: "black",
      }));
      G.updateEachEdgeAttributes((_, att) => ({
        ...att,
        color: defaultColor,
      }));

      state.BNodes.forEach((x) => {
        G.updateNodeAttributes(x, (att) => ({
          ...att,
          color: BColor,
        }));
      });
      state.BEdges.forEach((x) => G.setEdgeAttribute(x, "color", BColor));

      state.G2Nodes.forEach((x) =>
        G.updateNodeAttributes(x, (att) => ({
          ...att,
          type: "border",
          borderColor: GColor,
        })),
      );
      state.GEdges.forEach((x) => G.setEdgeAttribute(x, "color", GColor));

      if (state.RNode !== "") {
        G.setNodeAttribute(state.RNode, "borderColor", RColor);
      }
      state.R2Nodes.forEach((x) =>
        G.updateNodeAttributes(x, (att) => ({
          ...att,
          type: "border",
          borderColor: RColor,
        })),
      );
      state.REdges.forEach((x) => G.setEdgeAttribute(x, "color", RColor));
      R.refresh();
    }
    function updateState() {
      const G = graph.value;
      if (state.RNode !== "") {
        state.R2Nodes = G.undirectedNeighbors(state.RNode);
        state.REdges = G.undirectedEdges(state.RNode);
      } else {
        state.R2Nodes = [];
        state.REdges = [];
      }

      {
        // Loop
        const sortedBs = state.BNodes.flatMap((x) =>
          G.undirectedEdges(x),
        ).sort();
        if (Array.isNonEmptyArray(sortedBs)) {
          state.BEdges = Array.group(sortedBs)
            .filter((xs) => xs.length == 2)
            .map((x) => x[0]!);
        } else state.BEdges = [];
      }

      state.G2Nodes = foldIntersect(
        state.BNodes.map((x) => G.undirectedNeighbors(x)),
      );
      state.GEdges = foldIntersect([
        foldUnion(state.BNodes.map((x) => G.undirectedEdges(x))),
        foldUnion(state.G2Nodes.map((x) => G.undirectedEdges(x))),
      ]);
      updateStyle();
    }
    R.addListener("clickNode", (e) => {
      if (state.BNodes.includes(e.node))
        state.BNodes = state.BNodes.filter((x) => x !== e.node);
      else state.BNodes.push(e.node);
      updateState();
    });
    R.addListener("enterNode", (e) => {
      state.RNode = e.node;
      updateState();
    });
    R.addListener("leaveNode", (e) => {
      state.RNode = "";
      updateState();
    });
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
