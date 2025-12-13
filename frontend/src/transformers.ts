import { Array as A, Array, pipe } from "effect";
import type { NodeAttributes } from "@/graphdb.ts";
import { transpose } from "@/functions.ts";
import { type IndexItem } from "@/schema/TecRefined.ts";
import * as Raw from "@/schema/TecAstSchema.ts";
import { TecQuery, TecType } from "@/schema/TecAstSchema.ts";

type NE<T> = Array.NonEmptyArray<T>;
export function indexItemsToTec(ids: A.NonEmptyArray<IndexItem>) {
  if (ids.some((id) => typeof id !== typeof ids[0]))
    throw new Error("IndexItem type mismatch");
  return pipe(
    ids,
    A.map((id) => {
      if (typeof id === "number") return Raw.TecInt.make({ int: id });
      else return Raw.TecStr.make({ str: id });
    }),
    (tecs) => Raw.TecList.make({ list: tecs }),
  );
}

export function nodeAttributesToQuery(
  entries: NE<NE<NodeAttributes>>,
): TecQuery {
  const tecTypesWithRangeParameters = pipe(
    entries,
    transpose,
    Array.map((nodesOfSameTypeName) => {
      const idRanges = pipe(
        nodesOfSameTypeName,
        Array.map((att) => att.ids),
        transpose,
        Array.map(indexItemsToTec),
      );
      return TecType.make({
        typeName: nodesOfSameTypeName[0].typeName,
        parameters: idRanges,
      });
    }),
  );

  const tecType0 = tecTypesWithRangeParameters[0];
  const _tail = Array.tailNonEmpty(tecTypesWithRangeParameters);
  if (!Array.isNonEmptyArray(_tail)) throw new Error("tail is empty");
  const tecType1 = _tail[0];
  const __tail = Array.tailNonEmpty(_tail);
  const initialQuery = TecQuery.make({
    op: ":-",
    left: tecType0,
    right: tecType1,
  });

  return pipe(
    __tail,
    Array.reduce(initialQuery, (query, tecType) =>
      TecQuery.make({ op: ":-", left: query, right: tecType }),
    ),
  );
}
