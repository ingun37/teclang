<script setup lang="ts">
import { DirectedGraph } from "graphology";
import * as C from "codec";
import * as E from "effect";
import Sigma from "sigma";
import type { NodeDisplayData } from "sigma/types";
import { random } from "graphology-layout";

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

  random.assign(G);
  return G;
}
const graph = shallowRef<MyGraph>(createMyGraph());

const renderer = shallowRef<Sigma<MyNA> | null>(null);

function visualize() {
  if (sigmaContainer.value === null) throw new Error("sigmaContainer is null");
  if (renderer.value === null) {
    renderer.value = new Sigma(graph.value, sigmaContainer.value, {
      nodeReducer() {
        const res: Partial<NodeDisplayData> = {};
        res.x = Math.random();
        res.y = Math.random();
        return res;
      },
    });
  } else {
    renderer.value.refresh();
    renderer.value.setGraph(graph.value);
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
        <v-sheet height="1000px" width="500px">
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
