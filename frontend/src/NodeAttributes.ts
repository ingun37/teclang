import { Array, Order } from "effect";
import { type IndexItem, IndexItemOrder } from "@/schema/IndexItem.ts";

export type NodeAttributes = {
  typeName: string;
  ids: Array.NonEmptyArray<IndexItem>;
  meta: any;
};
const someSemantics = [
  "Colorway",
  "Fabric",
  "Pantone",
  "Side",
  "Render",
  "Schematic",
  "Size",
  "Line",
];
const typeNameOrder: Order.Order<string> = (x, y) => {
  if (x === y) return 0;
  const xi = someSemantics.findIndex((a) => a === x);
  const yi = someSemantics.findIndex((a) => a === y);
  if (-1 < xi) {
    if (-1 < yi) return xi < yi ? -1 : 1;
    else return -1;
  } else {
    if (-1 < yi) return 1;
    else return Order.string(x, y);
  }
};

export const NodeAttributesOrder: Order.Order<NodeAttributes> = (x, y) => {
  const a = typeNameOrder(x.typeName, y.typeName);
  if (a !== 0) return a;
  return Array.getOrder(IndexItemOrder)(x.ids, y.ids);
};
