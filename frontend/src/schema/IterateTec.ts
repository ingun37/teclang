import type { TecAST, TecType } from "@/schema/TecAstSchema.ts";
import type { TheGraph } from "@/graphdb.ts";
import { TecSide } from "@/schema/TecEnum.ts";

export function* iterateTec(
  tecType: TecType,
  depth: number,
): Generator<TecType> {
  if (tecType.parameters.length <= depth) {
    yield tecType;
    return;
  }
  const p = tecType.parameters[depth]!;

  if (p.tag === "TecRngInt") {
    for (let i = p.fromI; i <= (p.toI ?? 10); i++) {
      let parameters = [...tecType.parameters];
      parameters.splice(depth, 1, { tag: "TecInt", int: i });
      const newTecType: TecType = {
        ...tecType,
        parameters,
      };
      yield* iterateTec(newTecType, depth + 1);
    }
  } else if (p.tag === "TecRngEnum") {
    for (let i = p.fromE; i <= (p.toE ?? 10); i++) {
      let parameters = [...tecType.parameters];
      const label = TecSide[i];
      parameters.splice(depth, 1, {
        tag: "TecType",
        typeName: label,
        parameters: [],
      });
      const newTecType: TecType = {
        ...tecType,
        parameters,
      };
      yield* iterateTec(newTecType, depth + 1);
    }
  } else {
    yield* iterateTec(tecType, depth + 1);
  }
}

function idxStr(tecType: TecAST) {
  switch (tecType.tag) {
    case "TecInt":
      return tecType.int.toString();
    case "TecStr":
      return tecType.str;
    case "TecType":
      if (tecType.parameters.length === 0) return tecType.typeName;
      else throw new Error("Invalid index type");
    default:
      throw new Error("Invalid index type");
  }
}
export function queryDB(db: TheGraph) {
  return function (tecType: TecType): { key: string; tecType: TecType } | null {
    const idxPart = tecType.parameters.map(idxStr).join("-");
    const key = `${tecType.typeName}-${idxPart}`;
    if (db.hasNode(key)) return { key, tecType };
    return null;
  };
}
