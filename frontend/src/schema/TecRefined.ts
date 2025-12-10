import { ParseResult, pipe, Schema as S } from "effect";
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
const transformSide = S.transformOrFail(Raw.TecType, Raw._TecSide, {
  strict: true,
  decode(x, _, ast) {
    return pipe(
      Raw.TecSide[x.typeName],
      S.decodeUnknownOption(Raw._TecSide),
      ParseResult.fromOption(
        () => new ParseResult.Type(ast, x, "Failed to encode tecside"),
      ),
    );
  },
  encode(x) {
    return ParseResult.succeed({
      tag: "TecType",
      typeName: Raw.TecSide[x],
      parameters: [],
    } as Raw.TecType);
  },
});

export const Render = Raw.TecType.pipe(
  S.compose(
    S.Struct({
      ...Raw.TecType.fields,
      typeName: S.Literal("Render"),
      parameters: S.Tuple(Raw.TecInt, transformSide),
    }),
  ),
);
export type Render = typeof Render.Type;
export const RefinedTecType = S.Union(Text, Pantone, Render);
export type RefinedTecType = typeof RefinedTecType.Type;
