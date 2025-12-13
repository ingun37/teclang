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

  const clique3s = Array.fromIterable(combination(g.nodes(), 3)).filter(
    ([x, y, z]) => {
      return (
        subG.areUndirectedNeighbors(x, y) &&
        subG.areUndirectedNeighbors(y, z) &&
        subG.areUndirectedNeighbors(z, x)
      );
    },
  );

  new Set(clique3s.flat()).forEach((x) => {
    subG.dropNode(x);
  });

  const clique2s = subG.undirectedEdges().map((e) => subG.extremities(e));

  new Set(clique2s.flat()).forEach((x) => {
    subG.dropNode(x);
  });

  // const clique1s = subG.nodes().map((x) => Array.make(x));

  return pipe(
    clique3s,
    Array.map(([x, y, z]) => Array.make(x, y, z)),
    Array.appendAll(clique2s),
    // Array.appendAll(clique1s),
  );
}
type NE<T> = Array.NonEmptyArray<T>;
export function transpose<A>(ass: NE<NE<A>>): NE<NE<A>> {
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
