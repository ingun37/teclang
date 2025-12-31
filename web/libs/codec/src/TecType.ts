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

export const TecSumName = S.String.pipe(S.brand("TecSumName"));
export type TecSumName = typeof TecSumName.Type;

export const TecSum = S.Struct({
  tecTypeName: TecSumName,
  classes: S.Array(TecClass),
});
export type TecSum = typeof TecSum.Type;
export const TecEnumValue = S.String.pipe(S.brand("TecEnumValue"));
export type TecEnumValue = typeof TecEnumValue.Type;

export const TecEnumName = S.String.pipe(S.brand("TecEnumName"));
export type TecEnumName = typeof TecEnumName.Type;

export const TecEnum = S.Struct({
  tecTypeName: TecEnumName,
  values: S.Array(TecEnumValue),
});

export type TecEnum = typeof TecEnum.Type;

export const TecIndexedClass = S.Struct({
  className: S.String,
  indexSet: S.Array(TecEnumName),
  paramType: TecParamType,
});
export type TecIndexedClass = typeof TecIndexedClass.Type;

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
            tecTypeName: TecEnumName.make(input.tecTypeName),
            values: input.classes.map((c) => TecEnumValue.make(c.className)),
          }),
        ),
        E.Either.mapLeft((x) => x.issue),
      );
    },
    encode(input) {
      return E.ParseResult.succeed(
        TecSum.make({
          tecTypeName: TecSumName.make(input.tecTypeName),
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
  indexedClasses: S.Array(TecIndexedClass),
});
export type TecSchema = typeof TecSchema.Type;

const decodeEnums = (enums: TecSum[]) =>
  E.pipe(
    enums,
    E.Array.map((x) => S.decodeEither(TecEnumFromSum)(x)),
    E.Either.all,
  );
function makeIndexedClass(tecEnumsTable: TecEnum[]) {
  return function (tecClass: TecClass) {
    if (E.Array.isNonEmptyReadonlyArray(tecClass.parameterTypes)) {
      const [enumParams, typeParam] = E.Array.unappend(tecClass.parameterTypes);
      return E.pipe(
        enumParams,
        E.Array.map((enumParam) =>
          E.Array.findFirst(
            tecEnumsTable,
            (tecEnumEntry) =>
              (tecEnumEntry.tecTypeName as string) === (enumParam as string),
          ).pipe(
            E.Either.fromOption(() =>
              E.ParseResult.parseError(
                new E.ParseResult.Unexpected(
                  tecEnumsTable,
                  `Can't find enum for: ${enumParam}`,
                ),
              ),
            ),
          ),
        ),
        E.Either.all,
        E.Either.map((enums) => {
          return TecIndexedClass.make({
            className: tecClass.className,
            indexSet: enums.map((e) => e.tecTypeName),
            paramType: typeParam,
          });
        }),
      );
    } else
      return E.Either.left(
        E.ParseResult.parseError(
          new E.ParseResult.Unexpected(
            tecClass,
            "parameterTypes must be non empty array",
          ),
        ),
      );
  };
}
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
            E.Either.andThen((enums) => {
              return E.pipe(
                sum.classes,
                E.Array.map(makeIndexedClass(enums)),
                E.Either.all,
                E.Either.map((indexedClasses) => {
                  return TecSchema.make({ tecEnums: enums, indexedClasses });
                }),
              );
            }),
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
        E.Either.andThen((sums) => {
          return S.decodeEither(TecType)({
            sumTypes: E.Array.append(
              sums,
              TecSum.make({
                tecTypeName: TecSumName.make("TecType"),
                classes: tecSchema.indexedClasses.map((x) =>
                  TecClass.make({
                    className: x.className,
                    parameterTypes: x.indexSet
                      .concat([x.paramType])
                      .map((x) => TecParamType.make(x)),
                  }),
                ),
              }),
            ),
          });
        }),
        E.Either.mapLeft((x) => x.issue),
      );
    },
  }),
);
