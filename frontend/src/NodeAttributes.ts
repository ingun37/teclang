import { Array, Order } from "effect";
import { type IndexItem, IndexItemOrder } from "@/schema/IndexItem.ts";

export type NodeAttributes = {
  typeName: string;
  ids: Array.NonEmptyArray<IndexItem>;
  meta: any;
};
export const NodeAttributesOrder: Order.Order<NodeAttributes> = (x, y) => {
  const a = Order.string(x.typeName, y.typeName);
  if (a !== 0) return a;
  return Array.getOrder(IndexItemOrder)(x.ids, y.ids);
};
