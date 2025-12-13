import { Equivalence, Order } from "effect";

export type IndexItem = number | string;
export const IndexItemOrder: Order.Order<IndexItem> = (x, y) => {
  if (typeof x === "number") {
    if (typeof y === "number") return Order.number(x, y);
    else return -1;
  } else {
    if (typeof y === "number") return 1;
    else return Order.string(x, y);
  }
};
export const IndexItemEq: Equivalence.Equivalence<IndexItem> = (x, y) =>
  x === y;
