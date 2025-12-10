import Graph from "graphology";
import { Array } from "effect";

export type TheGraph = Graph<any, EdgeAttributes, any>;
export type EdgeAttributes = {
  edgeNode: string;
  thirdNodes: string[];
};
export type NodeAttributes =
  | {
      _tag: "TypeNode";
      typeName: string;
      index: string | number;
      index1?: string | number;
      attributes: any;
    }
  | {
      _tag: "EdgeNode";
    };
export function createGraphDB(): TheGraph {
  const graph = new Graph<NodeAttributes, EdgeAttributes, any>();

  const rng = (n: number) => Array.range(0, n - 1);
  const r3 = rng(3);

  function addNode(
    id: string | number,
    typeName: string,
    attributes: any = undefined,
    id1: string | number | undefined = undefined,
  ) {
    let node = `${typeName}-${id}`;
    if (id1 !== undefined) node = `${node}-${id1}`;
    return graph.addNode(`${node}`, {
      _tag: "TypeNode",
      typeName,
      index: id,
      attributes,
      index1: id1,
    });
  }

  const addColorway = (id: number) => addNode(id, "Colorway");
  const colorwayNodes = r3.map(addColorway);
  const addFabric = (id: string) => addNode(id, "Fabric");
  const fabrics = ["A", "B", "C", "D"];
  const fabricNodes = fabrics.map(addFabric);
  const addPantone = (id: string) => addNode(id, "Pantone");
  const pantones = ["r", "g", "b", "a"];
  const pantoneNodes = pantones.map(addPantone);
  const sides = ["Front", "Back", "Left", "Right"];
  const addRender = (id0: number, id1: number) =>
    addNode(id0, "Render", undefined, id1);
  const renderNodes = r3.map((id0) => sides.map((id1) => addRender(id0, id1)));

  function addEdge(
    from: string,
    to: string,
    third: string | undefined = undefined,
  ) {
    const xy = [from, to];
    xy.sort();
    const edgeNode = xy.join("->");
    const edgeAtt: EdgeAttributes = {
      edgeNode,
      thirdNodes: third ? [third] : [],
    };
    graph.addUndirectedEdge(from, to, edgeAtt);
    graph.addNode(edgeNode, {
      _tag: "EdgeNode",
    });

    edgeAtt.thirdNodes.forEach((thirdNode) =>
      graph.addEdge(edgeNode, thirdNode),
    );
  }

  r3.map((c) =>
    sides.map((s) =>
      addEdge(
        colorwayNodes[c]!,
        renderNodes[c]![sides.findIndex((x) => x === s)]!,
      ),
    ),
  );

  addEdge(colorwayNodes[0]!, fabricNodes[0]!, pantoneNodes[0]);
  addEdge(colorwayNodes[1]!, fabricNodes[0]!, pantoneNodes[1]);
  addEdge(colorwayNodes[2]!, fabricNodes[0]!, pantoneNodes[2]);

  addEdge(colorwayNodes[1]!, fabricNodes[1]!);
  addEdge(colorwayNodes[2]!, fabricNodes[2]!);
  addEdge(colorwayNodes[2]!, fabricNodes[3]!);

  console.log("=== MOCK DATA ====", { colorwayNodes, fabricNodes });
  return graph;
}
