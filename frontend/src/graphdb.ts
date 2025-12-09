import Graph from "graphology";
import { Array } from "effect";

export type Side = "Front" | "Back" | "Left" | "Right";
export type EdgeType = "index0";
export function createGraphDB(): Graph {
  const graph = new Graph();

  const rng = (n: number) => Array.range(0, n - 1);
  const r3 = rng(3);
  const colorwayNodes = r3.map((i) => graph.addNode(`Colorways-${i}`));

  const fabricNames = [
    "ECOALF 987.1",
    "BEEBLEBROX 3rd",
    "ARTHUR DENT",
    "MARVIN 1000x",
  ];
  const fabricNodes = fabricNames.map((i) => graph.addNode(`Fabric-${i}`));

  graph.addEdge(colorwayNodes[0], fabricNodes[0]);
  graph.addEdge(colorwayNodes[1], fabricNodes[0]);
  graph.addEdge(colorwayNodes[1], fabricNodes[1]);
  graph.addEdge(colorwayNodes[2], fabricNodes[2]);
  graph.addEdge(colorwayNodes[2], fabricNodes[3]);

  console.log("=== MOCK DATA ====", { colorwayNodes, fabricNodes });
  return graph;
}
