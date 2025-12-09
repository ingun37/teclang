import Graph from "graphology";
import { Array } from "effect";

export type TheGraph = Graph<any, EdgeAttributes, any>;
export type EdgeAttributes = {
  edgeNode: string;
  nodes: string[];
};
export type NodeAttributes = {
  typeName: string;
  attributes: any;
};
export function createGraphDB(): TheGraph {
  const graph = new Graph<NodeAttributes, EdgeAttributes, any>();

  const rng = (n: number) => Array.range(0, n - 1);
  const r3 = rng(3);

  function addNode(id: any, typeName: string, attributes: any = undefined) {
    return graph.addNode(`${typeName}-${id}`, {
      typeName,
      attributes,
    });
  }

  const addColorway = (id: any) => addNode(id, "Colorways");
  const colorwayNodes = r3.map(addColorway);
  const addFabric = (id: any) => addNode(id, "Fabric");
  const fabrics = ["A", "B", "C", "D"];
  const fabricNodes = fabrics.map(addFabric);
  const addPantone = (id: any) => addNode(id, "Pantone");
  const pantones = ["R", "G", "B", "A"];
  const pantoneNodes = pantones.map(addPantone);

  function addEdge(
    from: string,
    to: string,
    third: string | undefined = undefined,
  ) {
    const edgeNode = `${from}->${to}`;
    const edgeAtt: EdgeAttributes = {
      edgeNode,
      nodes: third ? [third] : [],
    };
    graph.addEdge(from, to, edgeAtt);
    graph.addNode(edgeNode);
    edgeAtt.nodes.forEach((thirdNode) => graph.addEdge(edgeNode, thirdNode));
  }

  addEdge(colorwayNodes[0]!, fabricNodes[0]!, pantoneNodes[0]);
  addEdge(colorwayNodes[1]!, fabricNodes[0]!, pantoneNodes[1]);
  addEdge(colorwayNodes[2]!, fabricNodes[0]!, pantoneNodes[2]);

  addEdge(colorwayNodes[1]!, fabricNodes[1]!);
  addEdge(colorwayNodes[2]!, fabricNodes[2]!);
  addEdge(colorwayNodes[2]!, fabricNodes[3]!);

  console.log("=== MOCK DATA ====", { colorwayNodes, fabricNodes });
  return graph;
}
