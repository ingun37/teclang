import type { TheGraph } from "@/graphdb.ts";
import { decodeGenericIndexSets, type IndexList, type IndexRange, type IndexSet } from "@/schema/TecRefined.ts";
import { Array, Effect, Equivalence, Order } from "effect";
import type { TecQuery, TecType } from "@/schema/TecAstSchema.ts";
import { type IndexItem, IndexItemOrder } from "@/schema/IndexItem.ts";
import type { NodeAttributes } from "@/NodeAttributes.ts";

function* iterateIndexSet(p: IndexSet): Generator<IndexItem> {
  switch (p._tag) {
    case "enum_set":
      yield* p.enums;
      break;
    case "string_set":
      yield* p.strings;
      break;
    case "int_list":
      yield* p.list;
      break;
    case "int_range":
      yield* Array.range(p.from, p.to ?? 10);
      break;
  }
}
export function* itertateIndexSets(
  parameters: readonly IndexSet[],
): Generator<IndexItem[]> {
  if (parameters.length === 1) {
    for (const i of iterateIndexSet(parameters[0]!)) yield [i];
    return;
  }
  if (0 < parameters.length) {
    for (const i of iterateIndexSet(parameters[0]!))
      for (const tail of itertateIndexSets(parameters.slice(1)))
        yield [i, ...tail];
    return;
  }
}

type EntryTT<T extends readonly IndexItem[]> = {
  node: string;
  indexSet: T;
  attributes: NodeAttributes;
};
export type EntryT<T extends IndexItem> = EntryTT<readonly T[]>;
export type Entry = EntryT<IndexItem>;
export type TypedEntry = { entry: Entry; typeName: string };
export const typedEntryOrder: Order.Order<TypedEntry> = (x, y) => {
  if (x.typeName !== y.typeName) return Order.string(x.typeName, y.typeName);
  return Array.getOrder(IndexItemOrder)(x.entry.indexSet, y.entry.indexSet);
};
export const typedEntryEq: Equivalence.Equivalence<TypedEntry> = (x, y) =>
  typedEntryOrder(x, y) === 0;
type IndexType<I extends IndexSet> = I extends IndexRange
  ? number
  : I extends IndexList
    ? number
    : string;
export function* iterateIndexSetsDBTuple<
  A extends IndexSet,
  B extends IndexSet,
>(
  db: TheGraph,
  typeName: string,
  parameters: readonly [A, B],
): Generator<EntryTT<readonly [IndexType<A>, IndexType<B>]>> {
  yield* iterateIndexSetsDB(db, typeName, parameters) as any;
}
export function* iterateIndexSetsDB(
  db: TheGraph,
  typeName: string,
  parameters: readonly IndexSet[],
): Generator<Entry> {
  const total = Array.fromIterable(itertateIndexSets(parameters));

  const nodes = Array.fromIterable(db.nodeEntries()).filter((entry) => {
    if (entry.attributes.typeName !== typeName) return false;
    return total.some((expIds) =>
      Array.zip(expIds, entry.attributes.ids).every(([a, b]) => {
        if (a === b) return true;
        if (typeof a === "string" && typeof b === "string")
          return new RegExp(a).test(b.toString());
        return false;
      }),
    );
  });

  const validIds = nodes.flatMap((entry): Entry[] => {
    return [
      {
        node: entry.node,
        indexSet: entry.attributes.ids,
        attributes: entry.attributes,
      },
    ];
  });
  // .flatMap((x) => (x._tag === "TypeNode" ? [x] : []))
  // .map((x): Entry => ({ indexSet: x.ids }));

  yield* validIds;
}

export function* iterateTecType(
  db: TheGraph,
  tecType: TecType,
): Generator<TypedEntry> {
  const indexSets: IndexSet[] = Effect.runSync(
    decodeGenericIndexSets(tecType.parameters),
  );
  const entries = iterateIndexSetsDB(db, tecType.typeName, indexSets);
  for (const entry of entries) yield { entry, typeName: tecType.typeName };
}

type NE<T> = Array.NonEmptyArray<T>;
export function* iterateQuery(
  db: TheGraph,
  query: TecQuery,
): Generator<NE<TypedEntry>> {
  if (query.left.tag === "TecType") {
    const rightDB = Array.fromIterable(iterateTecType(db, query.right));
    for (const leftEntry of iterateTecType(db, query.left)) {
      const neighbors = db.undirectedNeighbors(leftEntry.entry.node);
      const intersect = rightDB.filter((rightEntry) =>
        neighbors.includes(rightEntry.entry.node),
      );
      for (const rightEntry of intersect) {
        yield [leftEntry, rightEntry];
      }
    }
  } else {
    for (const chain of iterateQuery(db, query.left)) {
      const rightDB = Array.fromIterable(iterateTecType(db, query.right));

      for (const rightEntry of rightDB) {
        const inLoop = chain.every((leftEntry) => {
          const neighbors = db.undirectedNeighbors(leftEntry.entry.node);
          return neighbors.includes(rightEntry.entry.node);
        });

        if (inLoop) yield [...chain, rightEntry];
      }
    }
  }
}

export function* iterateTypeNames(db: TheGraph): Generator<string> {
  yield* new Set(
    Array.fromIterable(db.nodeEntries()).map((x) => x.attributes.typeName),
  );
}
