import { Schema as S } from "effect";
import * as Raw from "./TecAstSchema";
import { _TecSide } from "@/schema/TecEnum.ts";

export const Text = Raw.TecType.pipe(
  S.compose(
    S.Struct({
      ...Raw.TecType.fields,
      typeName: S.Literal("Text"),
      parameters: S.NonEmptyArray(Raw.TecStr),
    }),
  ),
);

export const Pantone = Raw.TecType.pipe(
  S.compose(
    S.Struct({
      ...Raw.TecType.fields,
      typeName: S.Literal("Pantone"),
      parameters: S.NonEmptyArray(Raw.TecStr),
    }),
  ),
);
export type Pantone = typeof Pantone.Type;

export const Render = Raw.TecType.pipe(
  S.compose(
    S.Struct({
      ...Raw.TecType.fields,
      typeName: S.Literal("Render"),
      parameters: S.Tuple(Raw.TecInt, _TecSide),
    }),
  ),
);
export type Render = typeof Render.Type;
export const RefinedTecType = S.Union(Text, Pantone, Render);
export type RefinedTecType = typeof RefinedTecType.Type;
