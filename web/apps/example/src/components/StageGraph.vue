<script setup lang="ts">
import { DirectedGraph } from "graphology";
import * as C from "codec";
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
  indexCombo: C.help.IndexEnumValueCombo;
};
type MyNA = MyEnumA | MyPnumA;
type MyGraph = DirectedGraph<MyNA>;
const props = defineProps<{
  tecClassAst: C.TecClass.TecClassAST;
  tecEnumAst: C.TecEnum.TecEnumAST;
  tecNodeAst: C.TecNode.TecNodeAST;
}>();

function combineEnumNodeName(enumName: string, enumValue: string) {
  return `${enumName}_${enumValue}`;
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

  props.tecEnumAst.tecEnums.forEach((tecEnum) => {
    const namer = makeEnumNodeName(tecEnum);
    const attr = makeEnumNodeAttr(tecEnum);
    tecEnum.tecEnumValues.forEach((tecEnumValue) => {
      G.addNode(namer(tecEnumValue), attr(tecEnumValue));
    });
  });
  props.tecNodeAst.tecNodeSets.forEach((tecNodeSet) => {
    const tc = findClass.force(tecNodeSet.tecNodeClass);
    const iter = C.help.iterateIndexCombo(
      props.tecEnumAst,
      tc.tecSignature.indexTypeSet,
    );
    tecNodeSet.tecNodeSet.forEach((tecNode) => {
      const idxCombos = iter.either(tecNode.indexCombination);
      if (idxCombos._tag === "Left")
        throw new Error(
          `Failed to iterate index combo for tecNode: ${JSON.stringify(idxCombos.left)}`,
        );
      for (const idxCombo of idxCombos.right) {
        const uniqueName = `${tc.tecClassName}_${idxCombo.join("-")}`;
        const att: MyPnumA = {
          tag: "pnum",
          tecNode,
          indexCombo: idxCombo,
          tecNodeSet: tecNodeSet,
        };
        if (!G.hasNode(uniqueName)) {
          G.addNode(uniqueName, att);
          tc.tecSignature.indexTypeSet.forEach((indexType, idx) => {
            G.addDirectedEdge(
              combineEnumNodeName(indexType, idxCombo[idx]!),
              uniqueName,
            );
          });
        } else {
          // overwrite
          G.setAttribute(uniqueName, att);
        }
      }
    });
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
