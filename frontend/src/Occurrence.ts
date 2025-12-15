import { Array } from "effect";
import type { IndexItem } from "@/schema/IndexItem.ts";

export type Occurrence = {
  all: Array.NonEmptyArray<IndexItem>;
};
export type AllOccurrences = {
  typeName: string;
  occurrences: Array.NonEmptyArray<Occurrence>;
};
