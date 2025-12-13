import Graph from "graphology";
import { Array } from "effect";
import { type IndexItem } from "@/schema/IndexItem.ts";
import type { NodeAttributes } from "@/NodeAttributes.ts";

export type EdgeAttributes = {
  edgeNode: string;
};

export type TheGraph = Graph<NodeAttributes, EdgeAttributes, any>;

export function createGraphDB(): TheGraph {
  const graph = new Graph<NodeAttributes, EdgeAttributes, any>();

  const rng = (n: number) => Array.range(0, n - 1);

  function addNode(
    typeName: string,
    ids: Array.NonEmptyArray<IndexItem>,
    meta?: any,
  ) {
    const idPart = ids.map((x) => x.toString()).join("-");
    let node = `${typeName}-${idPart}`;
    return graph.addNode(`${node}`, {
      typeName,
      ids,
      meta: meta ?? {},
    });
  }
  function addEdge(from: string, to: string, third?: string) {
    graph.addUndirectedEdge(from, to);
    if (third) {
      if (!graph.hasUndirectedEdge(from, third))
        graph.addUndirectedEdge(from, third);
      if (!graph.hasUndirectedEdge(to, third))
        graph.addUndirectedEdge(to, third);
    }
  }
  const colorways = rng(4);
  const addColorway = (id: number) => addNode("Colorway", [id]);
  const colorwayNodes = colorways.map(addColorway);
  const addFabric = (id: string) => addNode("Fabric", [id]);
  const fabrics = ["A", "B", "C", "D"];
  const fabricNodes = fabrics.map(addFabric);
  const addPantone = ([id, color]: [string, string]) =>
    addNode("Pantone", [id], { color });
  const pantones: [string, string][] = [
    ["white", "#ffffff"],
    ["black", "#000000"],
    ["khaki", "#424036"],
    ["red", "#b03435"],
  ];
  const pantoneNodes = pantones.map(addPantone);
  const sides = ["Front", "Back", "Left", "Right"];
  const sideNodes = sides.map((side) => addNode("Side", [side]));
  function sideNode(side: string) {
    const i = sides.findIndex((s) => s === side);
    if (i < 0) throw new Error(`side ${side} not found`);
    const n = sideNodes[i];
    if (!n) throw new Error(`side ${side} not found`);
    return n;
  }
  const addRender = (id0: number, id1: string) => {
    const node = addNode("Render", [id0, id1]);
    addEdge(sideNode(id1), node);
    return node;
  };
  const renderNodes = colorways.map((id0) =>
    sides.map((id1) => addRender(id0, id1)),
  );
  const schematicNodes = sides.map((side) => {
    const n = addNode("Schematic", [side]);
    addEdge(sideNode(side), n);
    return n;
  });

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
  addEdge(colorwayNodes[3]!, fabricNodes[0]!, pantoneNodes[3]!);

  addEdge(colorwayNodes[1]!, fabricNodes[1]!);
  addEdge(colorwayNodes[2]!, fabricNodes[2]!);
  addEdge(colorwayNodes[2]!, fabricNodes[3]!);

  const sizes = ["XS", "S", "M", "L", "XL", "2XL"];
  const addSize = (id: string) => addNode("Size", [id]);
  const sizeNodes = sizes.map(addSize);
  const lines = ["A", "B", "C", "D", "E"];
  const addLine = (id: string) => addNode("Line", [id]);
  const lineNodes = lines.map(addLine);
  const addPom = (size: string, line: string, value: number) =>
    addNode("Pom", [size, line], { value });
  const reference = [17, 4, 4.5, 17, 11];
  const pomNodes = sizes.map((size, i) =>
    lines.map((line, j) => {
      const pomNode = addPom(size, line, nonNull(reference[j]) + i * 0.5);
      addEdge(nonNull(sizeNodes[i]), pomNode);
      addEdge(nonNull(lineNodes[j]), pomNode);
      return pomNode;
    }),
  );

  return graph;
}
function nonNull<A>(a: A | null | undefined): A {
  if (a) return a;
  throw new Error("unexpected null");
}
