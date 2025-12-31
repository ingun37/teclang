import { Schema as S } from "effect";
import * as E from "effect";
import * as H from "./helper.js";
export const TecParamType = S.String.pipe(S.brand("TecParamType"));
export type TecParamType = typeof TecParamType.Type;

export const TecClass = S.Struct({
  className: S.String,
  parameterTypes: S.Array(TecParamType),
});
export type TecClass = typeof TecClass.Type;

export const TecSum = S.Struct({
  tecTypeName: S.String,
  classes: S.Array(TecClass),
});
export type TecSum = typeof TecSum.Type;
export const TecEnumValue = S.String.pipe(S.brand("TecEnumValue"));
export type TecEnumValue = typeof TecEnumValue.Type;
export const TecEnum = S.Struct({
  tecTypeName: S.String,
  values: S.Array(TecEnumValue),
});

export type TecEnum = typeof TecEnum.Type;

export const TecEnumFromSum = TecSum.pipe(
  S.transformOrFail(TecEnum, {
    strict: true,
    decode(input) {
      return E.pipe(
        input.classes,
        E.Array.map((c) =>
          S.decodeEither(S.Array(S.Any).pipe(S.itemsCount(0)))(
            c.parameterTypes,
          ),
        ),
        E.Either.all,
        E.Either.map((_) =>
          TecEnum.make({
            tecTypeName: input.tecTypeName,
            values: input.classes.map((c) => TecEnumValue.make(c.className)),
          }),
        ),
        E.Either.mapLeft((x) => x.issue),
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
export const TecSchemaFromTecType = TecType.pipe(
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
    encode(tecSchema) {
      return E.pipe(
        tecSchema.tecEnums,
        E.Array.map(H.commuteEncode(TecEnumFromSum, TecEnum)),
        E.Either.all,
        E.Either.andThen((sums) =>
          S.decodeEither(TecType)({
            sumTypes: E.Array.append(sums, tecSchema.tecSum),
          }),
        ),
        E.Either.mapLeft((x) => x.issue),
      );
    },
  }),
);
