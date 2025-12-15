import Graph from "graphology";
import { Array, flow, Order, pipe } from "effect";
import {
  combination,
  foldIntersect,
  foldUnion,
  iterateClique,
} from "@/functions.ts";
import { nonNull } from "@/nonnull.ts";
import { type GNode, gNodeOrder } from "@/GNode.ts";
import type { VGraph } from "@/VGraph.ts";

export type VState = ReturnType<VisualGraphState["computeState"]>;
export class VisualGraphState {
  private SelectedNodes: string[] = [];
  private HoveringNode: string = "";
  private HoveringEdge: string = "";
  private lastState: VState = {
    SelectedNodes: [],
    SingleNodes: [],
    SelectedEdges: [],
    AvailableNodes: [],
    AvailableEdges: [],
    preview: null,
    categories: null,
    HoveringEdge: "",
  };

  constructor(private readonly G: VGraph) {}

  getLastState() {
    return this.lastState;
  }

  toggleNode(node: string) {
    if (this.SelectedNodes.includes(node))
      this.SelectedNodes = this.SelectedNodes.filter((x) => x !== node);
    else this.SelectedNodes.push(node);
    return this.computeState();
  }

  hoverNode(node: string) {
    this.HoveringNode = node;
    return this.computeState();
  }

  unhoverNode() {
    this.HoveringNode = "";
    return this.computeState();
  }
  hoverEdge(edge: string) {
    this.HoveringEdge = edge;
    return this.computeState();
  }
  unhoverEdge() {
    this.HoveringEdge = "";
    return this.computeState();
  }
  private computeState() {
    const SelectedEdges = computeSelectedEdges(this.SelectedNodes, this.G);
    const AvailableNodes = computeAvailableNodes(this.SelectedNodes, this.G);
    const state = {
      SelectedNodes: this.SelectedNodes,
      SingleNodes: computeSingleNodes(this.SelectedNodes, this.G),
      SelectedEdges,
      AvailableNodes,
      AvailableEdges: computeAvailableEdges(
        AvailableNodes,
        this.SelectedNodes,
        this.G,
      ),
      preview:
        this.HoveringNode === ""
          ? null
          : computePreviewNodesAndEdges(this.G, {
              HoveringNode: this.HoveringNode,
              SelectedNodes: this.SelectedNodes,
              AvailableNodes: AvailableNodes,
            }),
      HoveringEdge: this.HoveringEdge,
      categories: computeEdgeCategories(
        this.SelectedNodes,
        SelectedEdges,
        this.G,
      ),
    };
    this.lastState = state;
    return state;
  }
}
type RNE<T> = Array.NonEmptyReadonlyArray<T>;
type NE<T> = Array.NonEmptyArray<T>;

function computeEdgeCategories(
  SelectedNodes: string[],
  SelectedEdges: string[],
  G: VGraph,
): null | {
  nodeCategories: RNE<RNE<RNE<GNode>>>;
  edgeCategories: NE<string[]>;
} {
  const subG = new Graph();

  SelectedNodes.forEach((n) => subG.addNode(n));
  SelectedEdges.forEach((e) => {
    const [x, y] = G.extremities(e);
    subG.addUndirectedEdgeWithKey(e, x, y);
  });

  const cliques = iterateClique(subG);

  const make = (n: string): GNode => ({
    attributes: G.getNodeAttributes(n).dbNodeAttributes,
    node: n,
  });
  const orderByJustTypeName: Order.Order<RNE<GNode>> = Array.getOrder(
    Order.mapInput((x: GNode) => x.attributes.typeName)(Order.string),
  );
  const equalByJustTypeName = flow(orderByJustTypeName, (x) => x === 0);
  if (!Array.isNonEmptyArray(cliques)) return null;
  const nodeCategories: RNE<RNE<RNE<GNode>>> = pipe(
    cliques,
    Array.map(Array.map(make)),
    Array.map(Array.sort(gNodeOrder)),
    Array.sort(orderByJustTypeName),
    Array.groupWith(equalByJustTypeName),
  );

  const edgeCategories: NE<string[]> = pipe(
    nodeCategories,
    Array.map(
      Array.flatMap((xxx) =>
        Array.fromIterable(
          combination(
            xxx.map((x) => x.node),
            2,
          ),
        ).map(([x, y]) => nonNull(G.undirectedEdge(x, y))),
      ),
    ),
  );

  return {
    nodeCategories,
    edgeCategories,
  };
}
function computeSingleNodes(SelectedNodes: string[], G: Graph): string[] {
  return SelectedNodes.filter((x) =>
    SelectedNodes.every((y) => !G.areUndirectedNeighbors(x, y)),
  );
}

function computeSelectedEdges(SelectedNodes: string[], G: Graph) {
  const sortedBs = SelectedNodes.flatMap((x) => G.undirectedEdges(x)).sort();
  if (Array.isNonEmptyArray(sortedBs)) {
    return Array.group(sortedBs)
      .filter((xs) => xs.length == 2)
      .map((x) => x[0]!);
  } else return [];
}

function computeAvailableNodes(SelectedNodes: string[], G: Graph) {
  return foldIntersect(SelectedNodes.map((x) => G.undirectedNeighbors(x)));
}
function computeAvailableEdges(
  AvailableNodes: string[],
  SelectedNodes: string[],
  G: Graph,
) {
  return foldIntersect([
    foldUnion(SelectedNodes.map((x) => G.undirectedEdges(x))),
    foldUnion(AvailableNodes.map((x) => G.undirectedEdges(x))),
  ]);
}

function computePreviewNodesAndEdges(
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
    isHoveringNodeSelect:
      Array.isEmptyArray(input.SelectedNodes) ||
      input.AvailableNodes.includes(input.HoveringNode),
    hoveringNode: input.HoveringNode,
    SelectPreviewNodes: good,
    SelectPreviewEdges: good.map(
      (x) => G.undirectedEdge(input.HoveringNode, x)!,
    ),
    UnknownPreviewNodes: unknown,
    UnknownPreviewEdges: unknownEdges,
  };
}
