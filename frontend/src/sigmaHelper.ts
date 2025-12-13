import { Array } from "effect";
import { foldIntersect, foldUnion } from "@/functions.ts";
import Sigma from "sigma";
import Graph from "graphology";
import parseColor from "color-parse";

function computeState1(G: Graph, input: { SelectedNodes: string[] }) {
  function computeSingleNodes() {
    return input.SelectedNodes.filter((x) =>
      input.SelectedNodes.every((y) => !G.areUndirectedNeighbors(x, y)),
    );
  }
  function computeSelectedEdges() {
    const sortedBs = input.SelectedNodes.flatMap((x) =>
      G.undirectedEdges(x),
    ).sort();
    if (Array.isNonEmptyArray(sortedBs)) {
      return Array.group(sortedBs)
        .filter((xs) => xs.length == 2)
        .map((x) => x[0]!);
    } else return [];
  }
  function computeAvailableNodes() {
    return foldIntersect(
      input.SelectedNodes.map((x) => G.undirectedNeighbors(x)),
    );
  }
  const AvailableNodes = computeAvailableNodes();
  function computeAvailableEdges() {
    return foldIntersect([
      foldUnion(input.SelectedNodes.map((x) => G.undirectedEdges(x))),
      foldUnion(AvailableNodes.map((x) => G.undirectedEdges(x))),
    ]);
  }
  return {
    SingleNodes: computeSingleNodes(),
    SelectedEdges: computeSelectedEdges(),
    AvailableNodes,
    AvailableEdges: computeAvailableEdges(),
  };
}

function computeState2(
  G: Graph,
  input: {
    HoveringNode: string;
    SelectedNodes: string[];
    AvailableNodes: string[];
  },
) {
  const allPreviewNodes = G.undirectedNeighbors(input.HoveringNode);
  const [unknown, good] = Array.partition(
    allPreviewNodes,
    (x) => input.AvailableNodes.includes(x) || input.SelectedNodes.includes(x),
  );
  const unknownEdges = unknown.map(
    (x) => G.undirectedEdge(input.HoveringNode, x)!,
  );
  return {
    SelectPreviewNodes: good,
    SelectPreviewEdges: good.map(
      (x) => G.undirectedEdge(input.HoveringNode, x)!,
    ),
    UnknownPreviewNodes: unknown,
    UnknownPreviewEdges: unknownEdges,
  };
}

export function configureSigma(
  R: Sigma,
  G: Graph,
  defaultColor: string,
  onSelect: (subGraph: Graph) => void,
) {
  const AvailableColor = "#00ff00";
  const SelectedColor = "#0000ff";
  const BadColor = "#ff0000";
  const styler = makeStyler(
    G,
    defaultColor,
    SelectedColor,
    AvailableColor,
    BadColor,
  );

  let SelectedNodes = [] as string[];
  function reset() {
    styler.reset();
    const state = computeState1(G, { SelectedNodes });
    SelectedNodes.forEach(styler.node.fillSelected);
    state.SingleNodes.forEach(styler.node.single);
    state.SelectedEdges.forEach(styler.edge.select);
    state.AvailableNodes.forEach(styler.node.borderGood);
    state.AvailableEdges.forEach(styler.edge.good);
    return state;
  }
  R.addListener("clickNode", (e) => {
    if (SelectedNodes.includes(e.node))
      SelectedNodes = SelectedNodes.filter((x) => x !== e.node);
    else SelectedNodes.push(e.node);

    const state = reset();
    const subG = new Graph();
    SelectedNodes.forEach((n) => subG.addNode(n));
    state.SelectedEdges.forEach((e) => {
      const [x, y] = G.extremities(e);
      console.log("connecting", x, y);
      subG.addUndirectedEdge(x, y);
    });
    try {
      onSelect(subG);
    } catch (e) {
      console.warn("onSelect exception is ignored");
      console.error(e);
    }
  });
  R.addListener("enterNode", (e) => {
    const state = reset();
    const state2 = computeState2(G, {
      HoveringNode: e.node,
      SelectedNodes,
      AvailableNodes: state.AvailableNodes,
    });

    state2.SelectPreviewNodes.forEach(styler.node.borderSelected);
    state2.SelectPreviewEdges.forEach(styler.edge.select);

    if (state.AvailableNodes.includes(e.node))
      styler.node.borderSelected(e.node);
    else if (Array.isEmptyArray(SelectedNodes))
      styler.node.borderSelected(e.node);
    if (Array.isEmptyArray(SelectedNodes)) {
      state2.UnknownPreviewNodes.forEach(styler.node.borderGood);
      state2.UnknownPreviewEdges.forEach(styler.edge.good);
    } else {
      state2.UnknownPreviewNodes.forEach(styler.node.borderBad);
      state2.UnknownPreviewEdges.forEach(styler.edge.bad);
    }
  });
  R.addListener("leaveNode", () => {
    reset();
  });
}

function makeStyler(
  G: Graph,
  defaultColor: string,
  selectedColor: string,
  goodColor: string,
  badColor: string,
) {
  const selectedColorValue = parseColor(selectedColor);
  if (selectedColorValue.space !== "rgb") throw new Error("expected rgb color");
  const selectedFillColor =
    "#" +
    selectedColorValue.values
      .map((x) =>
        Math.min(255, x + 128)
          .toString(16)
          .padStart(2, "0"),
      )
      .join("");
  return {
    reset() {
      G.updateEachNodeAttributes((_, att) => ({
        ...att,
        color: defaultColor,
        type: undefined,
        borderColor: "black",
      }));
      G.updateEachEdgeAttributes((_, att) => ({
        ...att,
        color: defaultColor,
        zIndex: 0,
      }));
    },
    edge: {
      select(e: string) {
        G.updateEdgeAttributes(e, (att) => ({
          ...att,
          color: selectedColor,
          zIndex: 3,
        }));
      },
      good(e: string) {
        G.updateEdgeAttributes(e, (att) => ({
          ...att,
          color: goodColor,
          zIndex: 2,
        }));
      },
      bad(e: string) {
        G.updateEdgeAttributes(e, (att) => ({
          ...att,
          color: badColor,
          zIndex: 1,
        }));
      },
    },
    node: {
      single(n: string) {
        G.updateNodeAttributes(n, (att) => ({
          ...att,
          type: undefined,
          color: "#eecc00",
        }));
      },
      borderGood(n: string) {
        G.updateNodeAttributes(n, (att) => ({
          ...att,
          type: "border",
          borderColor: goodColor,
        }));
      },
      borderSelected(n: string) {
        G.updateNodeAttributes(n, (att) => ({
          ...att,
          type: "border",
          borderColor: selectedColor,
        }));
      },
      borderBad(n: string) {
        G.updateNodeAttributes(n, (att) => ({
          ...att,
          type: "border",
          borderColor: badColor,
        }));
      },
      fillSelected(n: string) {
        G.updateNodeAttributes(n, (att) => ({
          ...att,
          type: undefined,
          color: selectedFillColor,
        }));
      },
    },
  };
}
