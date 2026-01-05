import { Schema as S } from "effect";
export const TecClassAttribute = S.String.pipe(S.brand("TecClassAttribute"));
export type TecClassAttribute = typeof TecClassAttribute.Type;
export const TecClassIndex = S.String.pipe(S.brand("TecClassIndex"));
export type TecClassIndex = typeof TecClassIndex.Type;

export const TecSignature = S.Struct({
  attributeTypeSet: S.NonEmptyArray(TecClassAttribute),
  indexTypeSet: S.Array(TecClassIndex),
});
export type TecSignature = typeof TecSignature.Type;
export const TecClassName = S.String.pipe(S.brand("TecClassName"));
export type TecClassName = typeof TecClassName.Type;

export const TecClass = S.Struct({
  tecClassName: TecClassName,
  tecSignature: TecSignature,
});
export type TecClass = typeof TecClass.Type;
export const TecClassAST = S.Struct({ tecClasses: S.Array(TecClass) });
export type TecClassAST = typeof TecClassAST.Type;
