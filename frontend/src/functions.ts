import { Array, Order, SortedSet } from "effect";

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
