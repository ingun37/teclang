<script setup lang="ts">
import { DirectedGraph } from "graphology";
import * as C from "codec";
import * as E from "effect";
import { SVG } from "@svgdotjs/svg.js";

type MyEnumA = {
  tag: "enum";
  enum: C.TecEnum.TecEnum;
  value: C.TecEnum.TecEnumValue;
};
type MyPnumA = {
  tag: "pnum";
  tecNode: C.TecNode.TecNode;
  tecNodeSet: C.TecNode.TecNodeSet;
  indexEnumValueCombo: C.help.IndexEnumValueCombo;
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
      E.Array.forEach(([indexEnumValueCombos, tecNode]) => {
        const tecEnums = E.Array.map(
          tc.tecSignature.indexTypeSet,
          findEnum.force,
        );
        for (const indexEnumValueCombo of indexEnumValueCombos) {
          const uniqueName = `p_${tc.tecClassName}_${indexEnumValueCombo.join("-")}`;
          const att: MyPnumA = {
            tag: "pnum",
            tecNode,
            indexEnumValueCombo,
            tecNodeSet,
          };
          G.addNode(uniqueName, att);

          indexEnumValueCombo.forEach((indexEnumValue, idx) => {
            G.addDirectedEdge(
              makeEnumNodeName(tecEnums[idx]!)(indexEnumValue),
              uniqueName,
            );
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
function visualize() {
  if (sigmaContainer.value === null) throw new Error("sigmaContainer is null");

  var draw = SVG()
    .addTo(sigmaContainer.value)
    .size(sheetWidth.value, sheetHeight.value);
  let offx = 0;

  const [enumNodes, pnumNodes] = E.Array.partitionMap(
    graph.value.nodeEntries(),
    (entry) => {
      const a = entry.attributes;
      if (a.tag === "enum") return E.Either.left(pair(entry.node, a));
      else return E.Either.right(pair(entry.node, a));
    },
  );
  if (E.Array.isNonEmptyArray(enumNodes)) {
    let offy = 50;
    E.pipe(
      enumNodes,
      E.Array.sortWith((x) => x[1].enum.tecEnumName, E.Order.string),
      E.Array.groupWith(
        (x, y) => x[1].enum.tecEnumName === y[1].enum.tecEnumName,
      ),
      E.Array.forEach((entries) => {
        for (const entry of entries) {
          draw.text(entry[1].value).attr({ x: offx, y: offy });
          offy += 30;
        }
      }),
    );
    offx += 300;
  }

  if (E.Array.isNonEmptyArray(pnumNodes)) {
    let offy = 50;

    E.pipe(
      pnumNodes,
      E.Array.sortWith((x) => x[1].tecNodeSet.tecNodeClass, E.Order.string),
      E.Array.groupWith(
        (x, y) => x[1].tecNodeSet.tecNodeClass === y[1].tecNodeSet.tecNodeClass,
      ),
      E.Array.forEach((entries) => {
        for (let i = 0; i < entries.length; i++) {
          const eA = entries[i]![1];
          draw
            .text(`${eA.tecNodeSet.tecNodeClass}_${i}`)
            .attr({ x: offx, y: offy });
          offy += 30;
        }
      }),
    );
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
