import type { NodeAttributes, TheGraph } from "@/graphdb.ts";
import { decodeGenericIndexSets, type IndexItem, type IndexSet } from "@/schema/TecRefined.ts";
import { Array, Effect } from "effect";
import type { TecQuery, TecType } from "@/schema/TecAstSchema.ts";

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

export type Entry = {
  node: string;
  indexSet: IndexItem[];
};
export type TypedEntry = { entry: Entry; typeName: string };

export function* iterateIndexSetsDB(
  db: TheGraph,
  typeName: string,
  parameters: readonly IndexSet[],
): Generator<Entry> {
  const total = Array.fromIterable(itertateIndexSets(parameters));

  const nodes = db.filterNodes((node, att: NodeAttributes) => {
    if (att._tag !== "TypeNode") return false;
    if (att.typeName !== typeName) return false;
    return total.some((expIds) =>
      Array.zip(expIds, att.ids).every(([a, b]) => {
        if (a === b) return true;
        if (typeof a === "string" && typeof b === "string")
          return new RegExp(a).test(b.toString());
        return false;
      }),
    );
  });

  const validIds = nodes.flatMap((node): Entry[] => {
    const na: NodeAttributes = db.getNodeAttributes(node);
    if (na._tag !== "TypeNode") return [];
    return [{ node, indexSet: na.ids }];
  });
  // .flatMap((x) => (x._tag === "TypeNode" ? [x] : []))
  // .map((x): Entry => ({ indexSet: x.ids }));

  yield* validIds;
}

export function* iterateTecType(
  db: TheGraph,
  tecType: TecType,
): Generator<TypedEntry> {
  const indexSets = Effect.runSync(decodeGenericIndexSets(tecType.parameters));
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
