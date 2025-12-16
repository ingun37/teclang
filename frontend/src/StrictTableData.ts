import type { TypedEntry } from "@/schema/IterateTec.ts";
import { Array } from "effect";

type NE<T> = Array.NonEmptyArray<T>;
export type StrictTableData = {
  headers: TypedEntry[];
  tails: NE<TypedEntry[]>;
};
