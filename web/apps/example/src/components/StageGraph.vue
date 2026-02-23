<script setup lang="ts">
import { DirectedGraph } from "graphology";
import * as C from "codec";
import * as E from "effect";
import { SVG, Element, type Svg, type NumberAlias } from "@svgdotjs/svg.js";
import { nonNull } from "@/nonnull.ts";

type MyEnumA = {
  tag: "enum";
  enum: C.TecEnum.TecEnum;
  value: C.TecEnum.TecEnumValue;
  svg?: Element;
};
type MyPnumA = {
  tag: "pnum";
  tecNode: C.TecNode.TecNode;
  tecNodeSet: C.TecNode.TecNodeSet;
  svg?: Element;
};
type MyNA = MyEnumA | MyPnumA;
type MyGraph = DirectedGraph<MyNA>;
const props = defineProps<{
  tecClassAst: C.TecClass.TecClassAST;
  tecEnumAst: C.TecEnum.TecEnumAST;
  tecNodeAst: C.TecNode.TecNodeAST;
}>();

function combineEnumNodeName(enumName: string, enumValue: string) {
  return `e_${enumName}_${enumValue}`;
}
function makeEnumNodeName(tecEnum: C.TecEnum.TecEnum) {
  return function (tecEnumValue: C.TecEnum.TecEnumValue) {
    return combineEnumNodeName(tecEnum.tecEnumName, tecEnumValue);
  };
}
function makeEnumNodeAttr(tecEnum: C.TecEnum.TecEnum) {
  return function (tecEnumValue: C.TecEnum.TecEnumValue): MyEnumA {
    return {
      tag: "enum",
      enum: tecEnum,
      value: tecEnumValue,
    };
  };
}
const sigmaContainer = useTemplateRef("sigma-container");
const sheetWidth = ref<number>(1000);
const sheetHeight = ref<number>(2000);

function createMyGraph(): MyGraph {
  const G: MyGraph = new DirectedGraph();
  const findClass = C.help.makeFindClass(props.tecClassAst);
  const findEnum = C.help.makeFindEnum(props.tecEnumAst);
  props.tecEnumAst.tecEnums.forEach((tecEnum) => {
    const namer = makeEnumNodeName(tecEnum);
    const attr = makeEnumNodeAttr(tecEnum);
    tecEnum.tecEnumValues.forEach((tecEnumValue) => {
      G.addNode(namer(tecEnumValue), attr(tecEnumValue));
    });
  });
  props.tecNodeAst.tecNodeSets.forEach((tecNodeSet) => {
    const tc = findClass.force(tecNodeSet.tecNodeClass);

    const iterator = C.help.iterateNodesInNodeSet(props.tecEnumAst, tc);

    const grouped = iterator.force(tecNodeSet.tecNodeSet);

    E.pipe(
      grouped,
      E.Array.zip(tecNodeSet.tecNodeSet),
      E.Array.forEach(([indexEnumValueCombos, tecNode], idx) => {
        const tecEnums = E.Array.map(
          tc.tecSignature.indexTypeSet,
          findEnum.force,
        );
        const pnumName = `p_${tc.tecClassName}_${idx}`;
        const att: MyPnumA = {
          tag: "pnum",
          tecNode,
          tecNodeSet,
        };
        G.addNode(pnumName, att);
        for (const indexEnumValueCombo of indexEnumValueCombos) {
          indexEnumValueCombo.forEach((indexEnumValue, idx) => {
            const enode = makeEnumNodeName(tecEnums[idx]!)(indexEnumValue);
            const edge = `${enode}__${pnumName}`;
            if (!G.hasDirectedEdge(edge))
              G.addDirectedEdgeWithKey(edge, enode, pnumName);
          });
        }
      }),
    );
  });

  return G;
}
const graph = shallowRef<MyGraph>(createMyGraph());
function pair<A, B>(a: A, b: B): [A, B] {
  return [a, b];
}

function createOutlinedTextbox(draw: Svg, x: number, y: number, text: string) {
  const paddingX = 8;
  const paddingY = 5;

  const g = draw.group();

  const t = g
    .text(text)
    .font({ family: "Roboto Mono, ui-monospace, monospace", size: 14 })
    .fill("#111111");

  // Position text inside the box first
  t.move(paddingX, paddingY);

  // Then size the box based on the rendered text bbox
  const tb = t.bbox();
  const r = g
    .rect(tb.width + paddingX * 2, tb.height + paddingY * 2)
    .fill("#ffffff")
    .stroke({ width: 1.5, color: "#111111" })
    .radius(4);

  r.back();
  g.move(x, y);

  return g;
}
function resolveNumberAlias(na: NumberAlias): number {
  if (typeof na === "number") return na;
  throw new Error("not number alias");
}
function visualize() {
  if (sigmaContainer.value === null) throw new Error("sigmaContainer is null");

  var draw = SVG()
    .addTo(sigmaContainer.value)
    .size(sheetWidth.value, sheetHeight.value);

  const [enumNodes, pnumNodes] = E.Array.partitionMap(
    graph.value.nodeEntries(),
    (entry) => {
      const a = entry.attributes;
      if (a.tag === "enum") return E.Either.left(pair(entry.node, a));
      else return E.Either.right(pair(entry.node, a));
    },
  );
  const hunit = 30;
  enumNodes.forEach((entry, idx) => {
    const eA = entry[1];
    eA.svg = createOutlinedTextbox(
      draw,
      0,
      hunit * idx - hunit * (enumNodes.length / 2),
      eA.value,
    );
  });

  pnumNodes.forEach((entry, idx) => {
    const eA = entry[1];

    eA.svg = createOutlinedTextbox(
      draw,
      500,
      hunit * idx - hunit * (pnumNodes.length / 2),
      entry[0],
    );
  });
  const maxY = Math.max(
    ...Array.from(graph.value.nodeEntries())
      .map((x) => x.attributes.svg!.y())
      .map(resolveNumberAlias),
  );

  for (const entry of graph.value.nodeEntries()) {
    entry.attributes.svg?.dmove(0, maxY + hunit * 2);
  }

  for (const edgeEntry of graph.value.directedEdgeEntries()) {
    const s = nonNull(edgeEntry.sourceAttributes.svg);
    const t = nonNull(edgeEntry.targetAttributes.svg);
    const sb = s.bbox();
    const tb = t.bbox();

    const sx = sb.x + sb.width;
    const sy = sb.y + sb.height / 2;
    const tx = tb.x;
    const ty = tb.y + tb.height / 2;

    draw.line(sx, sy, tx, ty).stroke({ width: 1, color: "#000000" });
  }
}
</script>

<template>
  <v-container fluid>
    <v-row>
      <v-col cols="12 ">
        <div class="d-flex flex-row ga-2">
          <v-btn @click="visualize">visualize</v-btn>
        </div>
      </v-col>
      <v-col cols="12">
        <v-sheet :height="sheetHeight" :width="sheetWidth">
          <div
            ref="sigma-container"
            class="sigma-container"
            style="width: 100%; height: 100%"
          ></div>
        </v-sheet>
      </v-col>
    </v-row>
  </v-container>
</template>

<style scoped lang="sass"></style>
