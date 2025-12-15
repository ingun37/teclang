import { Array } from "effect";
import Sigma from "sigma";
import Graph from "graphology";
import parseColor from "color-parse";
import { VisualGraphState, type VState } from "@/VisualGraphState.ts";
import type { VGraph } from "@/VGraph.ts";

export function configureSigma(
  R: Sigma,
  G: VGraph,
  defaultColor: string,
  onSelect: (state: VState) => void,
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

  const visualGraphState = new VisualGraphState(G);
  function reset(state: VState) {
    styler.reset();

    state.SelectedNodes.forEach(styler.node.fillSelected);
    state.SingleNodes.forEach(styler.node.single);
    state.SelectedEdges.forEach(styler.edge.select);
    state.AvailableNodes.forEach(styler.node.borderGood);
    state.AvailableEdges.forEach(styler.edge.good);

    if (state.preview) {
      state.preview.SelectPreviewNodes.forEach(styler.node.borderSelected);
      state.preview.SelectPreviewEdges.forEach(styler.edge.select);
      styler.node.borderSelected(state.preview.hoveringNode);
      if (Array.isEmptyArray(state.SelectedNodes)) {
        state.preview.UnknownPreviewNodes.forEach(styler.node.borderGood);
        state.preview.UnknownPreviewEdges.forEach(styler.edge.good);
      } else {
        state.preview.UnknownPreviewNodes.forEach(styler.node.borderBad);
        state.preview.UnknownPreviewEdges.forEach(styler.edge.bad);
      }
    }
    if (state.categories && state.HoveringEdge !== "") {
      const category = state.categories.edgeCategories.find((xs) =>
        xs.includes(state.HoveringEdge),
      );
      if (category) {
        category.forEach(styler.edge.sameCategory);
      }
    }

    return state;
  }
  R.addListener("clickNode", (e) => {
    onSelect(reset(visualGraphState.toggleNode(e.node)));
  });
  R.addListener("enterNode", (e) => {
    reset(visualGraphState.hoverNode(e.node));
  });
  R.addListener("leaveNode", () => {
    reset(visualGraphState.unhoverNode());
  });
  R.addListener("enterEdge", (event) => {
    reset(visualGraphState.hoverEdge(event.edge));
  });
  R.addListener("leaveEdge", () => {
    reset(visualGraphState.unhoverEdge());
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
        size: 1,
        zIndex: 0,
      }));
    },
    edge: {
      sameCategory(e: string) {
        G.updateEdgeAttributes(e, (att) => ({
          ...att,
          color: selectedColor,
          size: 4,
          zIndex: 4,
        }));
      },
      select(e: string) {
        G.updateEdgeAttributes(e, (att) => ({
          ...att,
          color: selectedFillColor,
          size: 3,

          zIndex: 3,
        }));
      },
      good(e: string) {
        G.updateEdgeAttributes(e, (att) => ({
          ...att,
          color: goodColor,
          size: 2,

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
