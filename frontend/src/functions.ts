import { Array, Order, pipe, SortedSet } from "effect";
import type Graph from "graphology";

export function foldIntersect(strings: string[][]): string[] {
  const sets = strings.map(SortedSet.fromIterable(Order.string));
  if (Array.isNonEmptyArray(sets)) {
    const tail = Array.tailNonEmpty(sets);
    const head = Array.headNonEmpty(sets);
    return Array.fromIterable(
      Array.reduce(tail, head, SortedSet.intersection<string>),
    );
  } else return [];
}

export function foldUnion(strings: string[][]): string[] {
  const sets = strings.map(SortedSet.fromIterable(Order.string));
  return Array.fromIterable(
    Array.reduce(sets, SortedSet.empty(Order.string), SortedSet.union<string>),
  );
}
export function combination<X>(xs: X[], n: 2): Generator<[X, X]>;
export function combination<X>(xs: X[], n: 3): Generator<[X, X, X]>;
export function combination<X>(xs: X[], n: number): Generator<X[]>;
export function* combination<X>(xs: X[], n: number): Generator<X[]> {
  if (xs.length < n) {
    return;
  }
  if (n === 0) {
    yield [];
    return;
  }

  for (let i = 0; i < xs.length - n + 1; i++) {
    for (const chain of combination(xs.slice(i + 1), n - 1))
      yield [xs[i]!, ...chain];
  }
}

export function iterateClique(g: Graph): NE<string>[] {
  const subG = g.copy();

  const clique3s = Array.fromIterable(combination(g.nodes(), 3)).flatMap(
    ([x, y, z]) => {
      const xy = subG.undirectedEdge(x, y);
      if (!xy) return [];
      const yz = subG.undirectedEdge(y, z);
      if (!yz) return [];
      const zx = subG.undirectedEdge(z, x);
      if (!zx) return [];
      return {
        nodes: Array.make(x, y, z),
        edges: Array.make(xy, yz, zx),
      };
    },
  );
  new Set(clique3s.flatMap((x) => x.edges)).forEach((x) => {
    subG.dropEdge(x);
  });

  const clique2s = subG.undirectedEdges().map((e) => subG.extremities(e));

  new Set(clique2s.flat()).forEach((x) => {
    subG.dropNode(x);
  });

  // const clique1s = subG.nodes().map((x) => Array.make(x));

  return pipe(
    clique3s,
    Array.map((x) => x.nodes),
    Array.appendAll(clique2s),
    // Array.appendAll(clique1s),
  );
}
type NE<T> = Array.NonEmptyArray<T>;
type RNE<T> = Array.NonEmptyReadonlyArray<T>;
export function transpose<A>(ass: RNE<RNE<A>>): RNE<RNE<A>> {
  const head = ass[0];
  if (ass.some((as) => head.length !== as.length)) {
    throw new Error("not square");
  }
  return pipe(
    head,
    Array.map((_, i) =>
      pipe(
        ass,
        Array.map((_, j) => ass[j]![i]!),
      ),
    ),
  );
}
