<script lang="ts" setup>
import { useAppStore } from "@/stores/app.ts";
import { iterateTypeNames } from "@/schema/IterateTec.ts";
import Graph from "graphology";
import Sigma from "sigma";
import { createNodeBorderProgram } from "@sigma/node-border";
import { Array, flow, Order, pipe } from "effect";
import { configureSigma } from "@/sigmaHelper.ts";
import { combination, iterateClique } from "@/functions.ts";
import { nodeAttributesToQuery } from "@/transformers.ts";
import type { TecQuery } from "@/schema/TecAstSchema.ts";
import TecLang from "@/components/TecLang.vue";
import { nonNull } from "@/nonnull.ts";
import {
  type GNode,
  gNodeEqById0,
  gNodeEqByTypeName,
  gNodeOrder,
} from "@/GNode.ts";

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
  const entries = Array.fromIterable(db.nodeEntries());
  if (!Array.isNonEmptyArray(entries)) throw new Error("no nodes in graphdb");
  const groupedByTypeName = pipe(
    entries,
    Array.sort(gNodeOrder),
    Array.groupWith(gNodeEqByTypeName),
  );

  const unitW = width / groupedByTypeName.length;
  const size = 8;
  const defaultColor = "#ccc";
  groupedByTypeName.forEach((entries, i) => {
    const groupedById0 = pipe(entries, Array.groupWith(gNodeEqById0));

    const hSize = groupedById0[0].length;
    const unitW2 = unitW / hSize;
    const vSize = groupedById0.length;
    const unitH = height / vSize;

    groupedById0.forEach((entries, j) => {
      entries.forEach((entry, i2) => {
        graph.value.addNode(entry.node, {
          x: unitW * i + unitW2 * i2 + unitW2 / 2,
          y: -unitH * j + (unitH * (vSize - 1)) / 2,
          size,
          label: entry.attributes.ids.map((x) => x.toString()).join("-"),
          color: defaultColor,
        });
      });
    });
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
  type RNE<T> = Array.NonEmptyReadonlyArray<T>;

  if (renderer.value) {
    let islandEdgesState: string[][] = [];
    configureSigma(
      renderer.value as any,
      graph.value,
      defaultColor,
      (subG) => {
        const cliques = iterateClique(subG);
        const make = (n: string): GNode => ({
          attributes: db.getNodeAttributes(n),
          node: n,
        });

        const orderByJustTypeName: Order.Order<RNE<GNode>> = Array.getOrder(
          Order.mapInput((x: GNode) => x.attributes.typeName)(Order.string),
        );

        const equalByJustTypeName = flow(orderByJustTypeName, (x) => x === 0);
        if (Array.isNonEmptyArray(cliques)) {
          const categories = pipe(
            cliques,
            Array.map(Array.map(make)),
            Array.map(Array.sort(gNodeOrder)),
            Array.sort(orderByJustTypeName),
            Array.groupWith(equalByJustTypeName),
          );

          islandEdgesState = pipe(
            categories,
            Array.map(
              Array.flatMap((xxx) =>
                Array.fromIterable(
                  combination(
                    xxx.map((x) => x.node),
                    2,
                  ),
                ).map(([x, y]) => nonNull(graph.value.undirectedEdge(x, y))),
              ),
            ),
          );

          queries.value = pipe(
            categories,
            Array.map(Array.map(Array.map((x) => x.attributes))),
            Array.map(nodeAttributesToQuery),
          );
        } else queries.value = [];
      },
      (e) => islandEdgesState.find((xs) => xs.includes(e)) ?? [],
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
