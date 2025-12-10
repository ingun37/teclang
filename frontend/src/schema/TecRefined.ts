import { Array as A, Either as E, ParseResult, pipe, Schema as S } from "effect";
import * as Raw from "./TecAstSchema";

const IndexRange = S.Struct({
  _tag: S.tag("int_range"),
  from: S.Number,
  to: S.NullOr(S.Number),
});
const IndexList = S.Struct({
  _tag: S.tag("int_list"),
  list: S.Array(S.Number),
});
const IntSet = S.transformOrFail(
  S.Union(Raw.TecInt, Raw.TecRngInt, Raw.TecList),
  S.Union(IndexRange, IndexList),
  {
    strict: true,
    decode(ints) {
      switch (ints.tag) {
        case "TecInt":
          return ParseResult.succeed(IndexList.make({ list: [ints.int] }));
        case "TecRngInt":
          return ParseResult.succeed(
            IndexRange.make({
              from: ints.fromI,
              to: ints.toI,
            }),
          );
        case "TecList":
          return pipe(
            ints.list,
            A.map((x) => S.decodeUnknownEither(Raw.TecInt)(x)),
            E.all,
            E.map((list) => IndexList.make({ list: list.map((x) => x.int) })),
            E.mapLeft((e) => e.issue),
          );
      }
    },
    encode(set) {
      switch (set._tag) {
        case "int_range":
          return ParseResult.succeed(
            Raw.TecRngInt.make({ fromI: set.from, toI: set.to }),
          );
        case "int_list":
          return ParseResult.succeed(
            Raw.TecList.make({
              list: set.list.map((x) => Raw.TecInt.make({ int: x })),
            }),
          );
      }
    },
  },
);
export type IntSet = typeof IntSet.Type;

const NullaryCon = S.transformOrFail(Raw.TecType, S.String, {
  decode(input) {
    return pipe(
      input.parameters,
      S.decodeUnknownEither(S.Array(S.Any).pipe(S.itemsCount(0))),
      E.map(() => input.typeName),
      E.mapLeft((e) => e.issue),
    );
  },
  encode(typeName) {
    return ParseResult.succeed(Raw.TecType.make({ typeName, parameters: [] }));
  },
});
const _EnumSet = S.Struct({
  _tag: S.tag("enum_set"),
  enums: S.Array(S.String),
});
const EnumSet = S.transformOrFail(S.Union(Raw.TecType, Raw.TecList), _EnumSet, {
  strict: true,
  decode(input) {
    switch (input.tag) {
      case "TecList":
        return pipe(
          input.list,
          A.map((x) => S.decodeUnknownEither(NullaryCon)(x)),
          E.all,
          E.map((enums) => _EnumSet.make({ enums })),
          E.mapLeft((e) => e.issue),
        );
      case "TecType":
        return pipe(
          input,
          S.decodeUnknownEither(NullaryCon),
          E.map((x) => _EnumSet.make({ enums: [x] })),
          E.mapLeft((e) => e.issue),
        );
    }
  },
  encode(enumSet) {
    return ParseResult.succeed(
      Raw.TecList.make({
        list: enumSet.enums.map((x) => S.encodeUnknownSync(NullaryCon)(x)),
      }),
    );
  },
});

export type EnumSet = typeof EnumSet.Type;
const _StringSet = S.Struct({
  _tag: S.tag("string_set"),
  strings: S.Array(S.String),
});
const StringSet = S.transformOrFail(
  S.Union(Raw.TecStr, Raw.TecList),
  _StringSet,
  {
    strict: true,
    decode(input) {
      switch (input.tag) {
        case "TecList":
          return pipe(
            input.list,
            A.map((x) => S.decodeUnknownEither(Raw.TecStr)(x)),
            E.all,
            E.map((x) => _StringSet.make({ strings: x.map((x) => x.str) })),
            E.mapLeft((e) => e.issue),
          );
        case "TecStr":
          return ParseResult.succeed(_StringSet.make({ strings: [input.str] }));
      }
    },
    encode(stringSet) {
      return ParseResult.succeed(
        Raw.TecList.make({
          list: stringSet.strings.map((str) => Raw.TecStr.make({ str })),
        }),
      );
    },
  },
);
export type StringSet = typeof StringSet.Type;
export type IndexSet = IntSet | EnumSet | StringSet;
export type IndexItem = number | string;
export const Text = Raw.TecType.pipe(
  S.compose(
    S.Struct({
      ...Raw.TecType.fields,
      typeName: S.Literal("Text"),
      parameters: S.Array(Raw.TecStr).pipe(S.itemsCount(1)),
    }),
  ),
);

export const Pantone = Raw.TecType.pipe(
  S.compose(
    S.Struct({
      ...Raw.TecType.fields,
      typeName: S.Literal("Pantone"),
      parameters: S.Array(StringSet).pipe(S.itemsCount(1)),
    }),
  ),
);
export type Pantone = typeof Pantone.Type;

export const Fabric = Raw.TecType.pipe(
  S.compose(
    S.Struct({
      ...Raw.TecType.fields,
      typeName: S.Literal("Fabric"),
      parameters: S.Array(StringSet).pipe(S.itemsCount(1)),
    }),
  ),
);
export type Fabric = typeof Fabric.Type;

export const Render = Raw.TecType.pipe(
  S.compose(
    S.Struct({
      ...Raw.TecType.fields,
      typeName: S.Literal("Render"),
      parameters: S.Tuple(IntSet, EnumSet),
    }),
  ),
);

export type Render = typeof Render.Type;
export const RefinedTecType = S.Union(Text, Pantone, Render, Fabric);
export type RefinedTecType = typeof RefinedTecType.Type;
