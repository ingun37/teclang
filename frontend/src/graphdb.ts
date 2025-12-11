import Graph from "graphology";
import { Array } from "effect";
import type { IndexItem } from "@/schema/TecRefined.ts";

export type TheGraph = Graph<any, EdgeAttributes, any>;
export type EdgeAttributes = {
  edgeNode: string;
  thirdNodes: string[];
};
export type NodeAttributes = {
  _tag: "TypeNode";
  typeName: string;
  ids: IndexItem[];
};
export function createGraphDB(): TheGraph {
  const graph = new Graph<NodeAttributes, EdgeAttributes, any>();

  const rng = (n: number) => Array.range(0, n - 1);

  function addNode(typeName: string, ...ids: IndexItem[]) {
    const idPart = ids.map((x) => x.toString()).join("-");
    let node = `${typeName}-${idPart}`;
    return graph.addNode(`${node}`, {
      _tag: "TypeNode",
      typeName,
      ids,
    });
  }
  const colorways = rng(4);
  const addColorway = (id: number) => addNode("Colorway", id);
  const colorwayNodes = colorways.map(addColorway);
  const addFabric = (id: string) => addNode("Fabric", id);
  const fabrics = ["A", "B", "C", "D"];
  const fabricNodes = fabrics.map(addFabric);
  const addPantone = (id: string) => addNode("Pantone", id);
  const pantones = ["r", "g", "b", "a"];
  const pantoneNodes = pantones.map(addPantone);
  const sides = ["Front", "Back", "Left", "Right"];
  const addRender = (id0: number, id1: string) => addNode("Render", id0, id1);
  const renderNodes = colorways.map((id0) =>
    sides.map((id1) => addRender(id0, id1)),
  );
  sides.map((side) => addNode("Schematic", side));

  function addEdge(from: string, to: string, third?: string) {
    graph.addUndirectedEdge(from, to);
    if (third) {
      if (!graph.hasUndirectedEdge(from, third))
        graph.addUndirectedEdge(from, third);
      if (!graph.hasUndirectedEdge(to, third))
        graph.addUndirectedEdge(to, third);
    }
  }

  colorways.map((c) =>
    sides.map((s) =>
      addEdge(
        colorwayNodes[c]!,
        renderNodes[c]![sides.findIndex((x) => x === s)]!,
      ),
    ),
  );

  addEdge(colorwayNodes[0]!, fabricNodes[0]!, pantoneNodes[0]!);
  addEdge(colorwayNodes[1]!, fabricNodes[0]!, pantoneNodes[1]!);
  addEdge(colorwayNodes[2]!, fabricNodes[0]!, pantoneNodes[2]!);
  addEdge(colorwayNodes[3]!, fabricNodes[0]!, pantoneNodes[2]!);

  addEdge(colorwayNodes[1]!, fabricNodes[1]!);
  addEdge(colorwayNodes[2]!, fabricNodes[2]!);
  addEdge(colorwayNodes[2]!, fabricNodes[3]!);

  console.log("=== MOCK DATA ====", { colorwayNodes, fabricNodes });
  return graph;
}
