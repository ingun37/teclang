import type Graph from "graphology";
import type { NodeAttributes } from "@/NodeAttributes.ts";

export type VNodeAttributes = {
  x: number;
  y: number;
  size: number;
  type: undefined | "border";
  borderColor: string;
  label: string;
  color: string;
  dbNodeAttributes: NodeAttributes;
};
export type VGraph = Graph<VNodeAttributes>;
