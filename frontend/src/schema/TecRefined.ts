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

export const RefinedTecType = S.Union(Text);
