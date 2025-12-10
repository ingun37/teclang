import type { NodeAttributes, TheGraph } from "@/graphdb.ts";
import type { IndexItem, IndexSet } from "@/schema/TecRefined.ts";
import { Array } from "effect";

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

export function* iterateIndexSetsDB(
  db: TheGraph,
  typeName: string,
  parameters: readonly IndexSet[],
) {
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

  const validIds = nodes
    .map((node): NodeAttributes => db.getNodeAttributes(node))
    .flatMap((x) => (x._tag === "TypeNode" ? [x] : []))
    .map((x) => x.ids);

  yield* validIds;
}
