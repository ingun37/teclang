import { Equivalence, Order } from "effect";

import { IndexItemEq } from "@/schema/IndexItem.ts";
import { type NodeAttributes, NodeAttributesOrder } from "@/NodeAttributes.ts";

export type GNode = { node: string; attributes: NodeAttributes };
export const gNodeOrder = Order.mapInput((x: GNode) => x.attributes)(
  NodeAttributesOrder,
);
export const gNodeEqByTypeName = Equivalence.mapInput(
  (x: GNode) => x.attributes.typeName,
)(Equivalence.string);

export const gNodeEqById0 = Equivalence.mapInput(
  (x: GNode) => x.attributes.ids[0],
)(IndexItemEq);
