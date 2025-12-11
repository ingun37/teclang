import { Schema as S } from "effect";
import type { Schema } from "effect/Schema";

export interface TecType {
  readonly tag: "TecType";
  readonly typeName: string;
  readonly parameters: readonly TecAST[];
}
export interface TecList {
  readonly tag: "TecList";
  readonly list: readonly TecAST[];
}
export interface TecQueryA {
  readonly tag: "TecQuery";
  readonly op: ":-";
  readonly left: TecType;
  readonly right: TecType;
}
export interface TecQueryB {
  readonly tag: "TecQuery";
  readonly op: ":>";
  readonly left: TecQuery;
  readonly right: TecType;
}
export type TecQuery = TecQueryA | TecQueryB;
export const TecInt = S.Struct({
  tag: S.tag("TecInt"),
  int: S.Number,
});
export const TecStr = S.Struct({
  tag: S.tag("TecStr"),
  str: S.String,
});
export const TecRngInt = S.Struct({
  tag: S.tag("TecRngInt"),
  fromI: S.Number,
  toI: S.NullOr(S.Number),
});

export type TecInt = typeof TecInt.Type;
export type TecStr = typeof TecStr.Type;
export type TecRngInt = typeof TecRngInt.Type;
export type TecAST = TecType | TecList | TecQuery | TecInt | TecStr | TecRngInt;

const TecQueryA = S.Struct({
  tag: S.tag("TecQuery"),
  op: S.Literal(":-"),
  left: S.suspend((): Schema<TecType> => TecType),
  right: S.suspend((): Schema<TecType> => TecType),
});
const TecQueryB = S.Struct({
  tag: S.tag("TecQuery"),
  op: S.Literal(":>"),
  left: S.suspend((): Schema<TecQuery> => TecQuery),
  right: S.suspend((): Schema<TecType> => TecType),
});

const TecQuery = S.Union(TecQueryA, TecQueryB);
export const TecType = S.Struct({
  tag: S.tag("TecType"),
  typeName: S.String,
  parameters: S.Array(S.suspend((): Schema<TecAST> => TecAST)),
});

export const TecList = S.Struct({
  tag: S.tag("TecList"),
  list: S.Array(S.suspend((): Schema<TecAST> => TecAST)),
});

export const TecAST = S.Union(
  S.suspend((): Schema<TecType> => TecType),
  S.suspend((): Schema<TecList> => TecList),
  S.suspend((): Schema<TecQuery> => TecQuery),
  TecInt,
  TecStr,
  TecRngInt,
);

export function decodeTecAST(o: any): TecAST {
  return S.decodeUnknownSync(TecAST)(o);
}
