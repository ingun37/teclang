<script lang="ts" setup>
import { useAppStore } from "@/stores/app.ts";
import { getAllNodesOfType, iterateTypeNames } from "@/schema/IterateTec.ts";
import Graph from "graphology";
import Sigma from "sigma";
import { createNodeBorderProgram } from "@sigma/node-border";

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
      A1Node: "",
      A2Nodes: [] as string[],
      A2Edges: [] as string[],
      B1Nodes: [] as string[],
      B2Nodes: [] as string[],
      B2Edges: [] as string[],
    };
    const B1Color = "#00ff00";
    const AColor = "#ff0000";
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

      state.B1Nodes.forEach((x) => {
        G.updateNodeAttributes(x, (att) => ({
          ...att,
          color: B1Color,
        }));
      });
      state.B2Nodes.forEach((x) =>
        G.updateNodeAttributes(x, (att) => ({
          ...att,
          type: "border",
          borderColor: B1Color,
        })),
      );
      state.B2Edges.forEach((x) => G.setEdgeAttribute(x, "color", B1Color));

      if (state.A1Node !== "") {
        G.setNodeAttribute(state.A1Node, "borderColor", AColor);
      }
      state.A2Nodes.forEach((x) =>
        G.updateNodeAttributes(x, (att) => ({
          ...att,
          type: "border",
          borderColor: AColor,
        })),
      );
      state.A2Edges.forEach((x) => G.setEdgeAttribute(x, "color", AColor));
      R.refresh();
    }
    function updateState() {
      const G = graph.value;
      if (state.A1Node !== "") {
        state.A2Nodes = G.undirectedNeighbors(state.A1Node);
        state.A2Edges = G.undirectedEdges(state.A1Node);
      } else {
        state.A2Nodes = [];
        state.A2Edges = [];
      }

      state.B2Nodes = state.B1Nodes.flatMap((b1) => G.undirectedNeighbors(b1));
      state.B2Edges = state.B1Nodes.flatMap((b1) => G.undirectedEdges(b1));
      updateStyle();
    }
    R.addListener("clickNode", (e) => {
      if (state.B1Nodes.includes(e.node))
        state.B1Nodes = state.B1Nodes.filter((x) => x !== e.node);
      else state.B1Nodes.push(e.node);
      updateState();
    });
    R.addListener("enterNode", (e) => {
      state.A1Node = e.node;
      updateState();
    });
    R.addListener("leaveNode", (e) => {
      state.A1Node = "";
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
