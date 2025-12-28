import { ParseResult, Schema as S } from "effect";
import * as E from "effect";
export const TecClass = S.Struct({
  className: S.String,
  parameterTypes: S.Array(S.String),
});
export type TecClass = typeof TecClass.Type;

export const TecSum = S.Struct({
  tecTypeName: S.String,
  classes: S.Array(TecClass),
});
export type TecSum = typeof TecSum.Type;

const TecEnum = S.Struct({
  tecTypeName: S.String,
  values: S.Array(S.String),
});

export type TecEnum = typeof TecEnum.Type;

export const TecEnumFromSum = TecSum.pipe(
  S.transformOrFail(TecEnum, {
    strict: true,
    decode(input, options, ast) {
      for (const c of input.classes) {
        if (0 < c.parameterTypes.length) {
          return E.ParseResult.fail(
            new ParseResult.Type(
              ast,
              c,
              "Class cannot have parameter types in an enum",
            ),
          );
        }
      }
      return E.ParseResult.succeed(
        TecEnum.make({
          tecTypeName: input.tecTypeName,
          values: input.classes.map((c) => c.className),
        }),
      );
    },
    encode(input) {
      return E.ParseResult.succeed(
        TecSum.make({
          tecTypeName: input.tecTypeName,
          classes: input.values.map((v) =>
            TecClass.make({ className: v, parameterTypes: [] }),
          ),
        }),
      );
    },
  }),
);

export const TecType = S.Struct({
  sumTypes: S.Array(TecSum),
});

export type TecType = typeof TecType.Type;
