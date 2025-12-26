import { Schema as S } from "effect";

const TecClass = S.Struct({
  className: S.String,
  parameterTypes: S.Array(S.String),
});
type TecClass = typeof TecClass.Type;

export const TecType = S.Struct({
  tecTypeName: S.String,
  classes: S.Array(TecClass),
});
export type TecType = typeof TecType.Type;
