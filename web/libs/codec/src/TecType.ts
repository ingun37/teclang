import { Schema as S } from "effect";
const TecSumSus = S.suspend((): S.Schema<TecSum> => TecSum);
const TecTypeSus = S.suspend((): S.Schema<TecType> => TecType);

interface TecSum {
  tag: "TecSum";
  readonly tecTypes: readonly TecType[];
}
const TecSum = S.Struct({
  tag: S.tag("TecSum"),
  tecTypes: S.Array(TecTypeSus),
});

const TecClass = S.Struct({
  tag: S.tag("TecClass"),
  className: S.String,
  parameterTypes: S.Array(S.String),
});
type TecClass = typeof TecClass.Type;

export type TecType = TecSum | TecClass;
export const TecType = S.Union(TecSumSus, TecClass);
