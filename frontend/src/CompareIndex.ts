import type { TecAST, TecRngInt } from "@/schema/TecAstSchema.ts";

export function compareIndex(idx: number | string, tec: TecAST) {
  if (typeof idx === "number") {
    if (tec.tag === "TecInt") return tec.int === idx;
    if (tec.tag === "TecRngInt")
      return tec.fromI <= idx && idx <= (tec.toI ?? 10);
    if (tec.tag === "TecRngEnum")
      return tec.fromE <= idx && idx <= (tec.toE ?? 10);
    return false;
  } else if (typeof idx === "string") {
    if (tec.tag === "TecStr") {
      if (tec.str === "*") return true;
      return tec.str === idx;
    }
    return false;
  } else {
    throw new Error("Invalid index type");
  }
}
