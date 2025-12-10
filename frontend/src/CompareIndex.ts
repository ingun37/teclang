import type { TecRngInt } from "@/schema/TecAstSchema.ts";

export function compareIndex(idx: number | string, tec: TecRngInt) {
  if (typeof idx === "number") {
    if (tec.tag === "IndexE") return tec.contents.value === idx;
    if (tec.tag === "IndexR")
      return tec.from.value <= idx && idx <= (tec.to?.value ?? 10);
    return false;
  } else if (typeof idx === "string") {
    if (tec.tag === "IndexS") {
      if (tec.name === "*") return true;
      return tec.name === idx;
    }
    return false;
  } else {
    throw new Error("Invalid index type");
  }
}
