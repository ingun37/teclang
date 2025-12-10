import { Schema as S } from "effect";
import type { Schema } from "effect/Schema";
import * as Raw from "./TecAstSchema";

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
export const RefinedTecType = S.Union(Text, Pantone);
export type RefinedTecType = typeof RefinedTecType.Type;
