import { Schema as S } from "effect";
export const TecEnumValue = S.String.pipe(S.brand("TecEnumValue"));
export type TecEnumValue = typeof TecEnumValue.Type;

export const TecEnumName = S.String.pipe(S.brand("TecEnumName"));
export type TecEnumName = typeof TecEnumName.Type;

export const TecEnumRepresentationAttribute = S.String.pipe(
  S.brand("TecEnumRepresentationAttribute"),
);
export type TecEnumRepresentationAttribute =
  typeof TecEnumRepresentationAttribute.Type;
export const TecEnumRepresentation = S.Struct({
  repAttribs: S.Array(TecEnumRepresentationAttribute),
});
export type TecEnumRepresentation = typeof TecEnumRepresentation.Type;
export const TecEnum = S.Struct({
  tecEnumName: TecEnumName,
  tecEnumRepresentation: TecEnumRepresentation,
  tecEnumValues: S.Array(TecEnumValue),
});
export type TecEnum = typeof TecEnum.Type;
export const TecEnumAST = S.Struct({ tecEnums: S.Array(TecEnum) });
export type TecEnumAST = typeof TecEnumAST.Type;
