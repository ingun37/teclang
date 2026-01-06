import { Schema as S } from "effect";
export const TecEnumValue = S.String.pipe(S.brand("TecEnumValue"));
export type TecEnumValue = typeof TecEnumValue.Type;

export const TecEnumName = S.String.pipe(S.brand("TecEnumName"));
export type TecEnumName = typeof TecEnumName.Type;

export const TecEnum = S.Struct({
  tecEnumName: TecEnumName,
  tecEnumValues: S.Array(TecEnumValue),
});
export type TecEnum = typeof TecEnum.Type;
export const TecEnumAST = S.Struct({ tecEnums: S.Array(TecEnum) });
export type TecEnumAST = typeof TecEnumAST.Type;
