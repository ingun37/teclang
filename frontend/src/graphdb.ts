import Graph from "graphology";
import { Array } from "effect";

export type Side = "Front" | "Back" | "Left" | "Right";
export type EdgeType = "index0";
export function createGraphDB(): Graph {
  const graph = new Graph();

  const rng = (n: number) => Array.range(0, n - 1);
  const r3 = rng(3);
  const r4 = rng(4);
  const colorwayNodes = r3.map((i) => graph.addNode(`Colorways-${i}`));
  const renderNodes = r3.map((i) =>
    r4.map((j) => graph.addNode(`Renders-${i}-${j}`)),
  );

  const edges = Array.zip(colorwayNodes, renderNodes).map(
    ([colorwayNode, renderNodes]) =>
      renderNodes.map((renderNode) =>
        graph.addEdge(colorwayNode, renderNode, { type: "index0" as EdgeType }),
      ),
  );
  console.log("=== MOCK DATA ====", { colorwayNodes, renderNodes, edges });
  return graph;
}
