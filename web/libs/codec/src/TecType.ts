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

export const TecEnum = S.Struct({
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
export const TecSchema = S.Struct({
  tecEnums: S.Array(TecEnum),
  tecSum: TecSum,
});
export type TecSchema = typeof TecSchema.Type;

const decodeEnums = (enums: TecSum[]) =>
  E.pipe(
    enums,
    E.Array.map((x) => S.decodeEither(TecEnumFromSum)(x)),
    E.Either.all,
  );
export const UniqueTecTypeFromTecType = TecType.pipe(
  S.transformOrFail(TecSchema, {
    strict: true,
    decode(input) {
      return E.pipe(
        input.sumTypes,
        S.decodeUnknownEither(S.NonEmptyArray(TecSum)),
        E.Either.andThen((sums) => {
          const [enums, sum] = E.Array.unappend(sums);
          return E.pipe(
            enums,
            decodeEnums,
            E.Either.map((enums) =>
              TecSchema.make({ tecEnums: enums, tecSum: sum }),
            ),
          );
        }),
        E.Either.mapLeft((x) => x.issue),
      );
    },
    encode(uniqueTT: TecSchema) {
      return E.pipe(
        uniqueTT.tecEnums,
        E.Array.map((x) => S.encodeEither(TecEnumFromSum)(x)),
        E.Either.all,
        E.Either.map((sums) =>
          TecType.make({ sumTypes: E.Array.append(sums, uniqueTT.tecSum) }),
        ),
        E.Either.mapLeft((x) => x.issue),
      );
    },
  }),
);
