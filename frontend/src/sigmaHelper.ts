import { Array } from "effect";
import { foldIntersect, foldUnion } from "@/functions.ts";
import Sigma from "sigma";
import type Graph from "graphology";

export function configureSigma(R: Sigma, G: Graph, defaultColor: string) {
  const state = {
    RNode: "",
    R2Nodes: [] as string[],
    REdges: [] as string[],
    BNodes: [] as string[],
    BEdges: [] as string[],
    G2Nodes: [] as string[],
    GEdges: [] as string[],
  };
  const GColor = "#00ff00";
  const BColor = "#0000ff";
  const RColor = "#ff0000";
  function updateStyle() {
    G.updateEachNodeAttributes((_, att) => ({
      ...att,
      color: defaultColor,
      type: undefined,
      borderColor: "black",
    }));
    G.updateEachEdgeAttributes((_, att) => ({
      ...att,
      color: defaultColor,
    }));

    state.BNodes.forEach((x) => {
      G.updateNodeAttributes(x, (att) => ({
        ...att,
        color: BColor,
      }));
    });
    state.BEdges.forEach((x) => G.setEdgeAttribute(x, "color", BColor));

    state.G2Nodes.forEach((x) =>
      G.updateNodeAttributes(x, (att) => ({
        ...att,
        type: "border",
        borderColor: GColor,
      })),
    );
    state.GEdges.forEach((x) => G.setEdgeAttribute(x, "color", GColor));

    if (state.RNode !== "") {
      G.setNodeAttribute(state.RNode, "borderColor", RColor);
    }
    state.R2Nodes.forEach((x) =>
      G.updateNodeAttributes(x, (att) => ({
        ...att,
        type: "border",
        borderColor: RColor,
      })),
    );
    state.REdges.forEach((x) => G.setEdgeAttribute(x, "color", RColor));
    R.refresh();
  }
  function updateState() {
    if (state.RNode !== "") {
      state.R2Nodes = G.undirectedNeighbors(state.RNode);
      state.REdges = G.undirectedEdges(state.RNode);
    } else {
      state.R2Nodes = [];
      state.REdges = [];
    }

    {
      // Loop
      const sortedBs = state.BNodes.flatMap((x) => G.undirectedEdges(x)).sort();
      if (Array.isNonEmptyArray(sortedBs)) {
        state.BEdges = Array.group(sortedBs)
          .filter((xs) => xs.length == 2)
          .map((x) => x[0]!);
      } else state.BEdges = [];
    }

    state.G2Nodes = foldIntersect(
      state.BNodes.map((x) => G.undirectedNeighbors(x)),
    );
    state.GEdges = foldIntersect([
      foldUnion(state.BNodes.map((x) => G.undirectedEdges(x))),
      foldUnion(state.G2Nodes.map((x) => G.undirectedEdges(x))),
    ]);
    updateStyle();
  }
  R.addListener("clickNode", (e) => {
    if (state.BNodes.includes(e.node))
      state.BNodes = state.BNodes.filter((x) => x !== e.node);
    else state.BNodes.push(e.node);
    updateState();
  });
  R.addListener("enterNode", (e) => {
    state.RNode = e.node;
    updateState();
  });
  R.addListener("leaveNode", (e) => {
    state.RNode = "";
    updateState();
  });
}
