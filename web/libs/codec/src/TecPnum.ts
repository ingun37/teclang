import { Schema as S } from "effect";
import * as E from "effect";
export const TecIndexValue = S.Struct({
  tag: S.tag("TecIndexValue"),
  contents: S.String,
});
export type TecIndexValue = typeof TecIndexValue.Type;
export const TecIndexAll = S.Struct({
  tag: S.tag("TecIndexAll"),
});
export type TecIndexAll = typeof TecIndexAll.Type;
export const TecIndexPattern = S.Union(TecIndexValue, TecIndexAll);
export type TecIndexPattern = typeof TecIndexPattern.Type;
export const TecMatch = S.Struct({
  _tecIndexPatterns: S.NonEmptyArray(TecIndexPattern),
  _tecEnumValue: S.String,
});
export type TecMatch = typeof TecMatch.Type;
export const TecPnumTable = S.Struct({
  _tecIndexTypes: S.NonEmptyArray(S.String),
  _tecMatches: S.NonEmptyArray(TecMatch),
});
export type TecPnumTable = typeof TecPnumTable.Type;
export const TecPnum = S.Struct({
  _tecPnumName: S.String,
  _tecEnumValues: S.NonEmptyArray(S.String),
  _tecPnumTable: S.NullOr(TecPnumTable),
});
export type TecPnum = typeof TecPnum.Type;
export const TecPnumAST = S.Struct({ tecPnums: S.Array(TecPnum) });
export type TecPnumAST = typeof TecPnumAST.Type;

type RNE<T> = E.Array.NonEmptyReadonlyArray<T>;
export const lens = {
  tecIndexValue: {
    view: {
      contents(x: TecIndexValue): string {
        return x.contents;
      },
    },
    over: {
      contents(f: (contents: string) => string) {
        return function (x: TecIndexValue): TecIndexValue {
          return TecIndexValue.make({
            tag: x.tag,
            contents: f(x.contents),
          });
        };
      },
    },
  },

  tecMatch: {
    view: {
      _tecIndexPatterns(x: TecMatch): RNE<TecIndexPattern> {
        return x._tecIndexPatterns;
      },
      _tecEnumValue(x: TecMatch): string {
        return x._tecEnumValue;
      },
    },
    over: {
      _tecIndexPatterns(
        f: (patterns: RNE<TecIndexPattern>) => RNE<TecIndexPattern>,
      ) {
        return function (x: TecMatch): TecMatch {
          return TecMatch.make({
            _tecIndexPatterns: f(x._tecIndexPatterns),
            _tecEnumValue: x._tecEnumValue,
          });
        };
      },
      _tecEnumValue(f: (value: string) => string) {
        return function (x: TecMatch): TecMatch {
          return TecMatch.make({
            _tecIndexPatterns: x._tecIndexPatterns,
            _tecEnumValue: f(x._tecEnumValue),
          });
        };
      },
    },
  },

  tecPnumTable: {
    view: {
      _tecIndexTypes(x: TecPnumTable): RNE<string> {
        return x._tecIndexTypes;
      },
      _tecMatches(x: TecPnumTable): RNE<TecMatch> {
        return x._tecMatches;
      },
    },
    over: {
      _tecIndexTypes(f: (types: RNE<string>) => RNE<string>) {
        return function (x: TecPnumTable): TecPnumTable {
          return TecPnumTable.make({
            _tecIndexTypes: f(x._tecIndexTypes),
            _tecMatches: x._tecMatches,
          });
        };
      },
      _tecMatches(f: (matches: RNE<TecMatch>) => RNE<TecMatch>) {
        return function (x: TecPnumTable): TecPnumTable {
          return TecPnumTable.make({
            _tecIndexTypes: x._tecIndexTypes,
            _tecMatches: f(x._tecMatches),
          });
        };
      },
    },
  },

  tecPnum: {
    view: {
      _tecPnumName(x: TecPnum): string {
        return x._tecPnumName;
      },
      _tecEnumValues(x: TecPnum): RNE<string> {
        return x._tecEnumValues;
      },
      _tecPnumTable(x: TecPnum): TecPnumTable | null {
        return x._tecPnumTable;
      },
    },
    over: {
      _tecPnumName(f: (name: string) => string) {
        return function (x: TecPnum): TecPnum {
          return TecPnum.make({
            _tecPnumName: f(x._tecPnumName),
            _tecEnumValues: x._tecEnumValues,
            _tecPnumTable: x._tecPnumTable,
          });
        };
      },
      _tecEnumValues(f: (values: RNE<string>) => RNE<string>) {
        return function (x: TecPnum): TecPnum {
          return TecPnum.make({
            _tecPnumName: x._tecPnumName,
            _tecEnumValues: f(x._tecEnumValues),
            _tecPnumTable: x._tecPnumTable,
          });
        };
      },
      _tecPnumTable(f: (table: TecPnumTable | null) => TecPnumTable | null) {
        return function (x: TecPnum): TecPnum {
          return TecPnum.make({
            _tecPnumName: x._tecPnumName,
            _tecEnumValues: x._tecEnumValues,
            _tecPnumTable: f(x._tecPnumTable),
          });
        };
      },
    },
  },

  tecPnumAst: {
    view: {
      tecPnums(x: TecPnumAST): ReadonlyArray<TecPnum> {
        return x.tecPnums;
      },
    },
    over: {
      tecPnums(
        f: (tecPnums: ReadonlyArray<TecPnum>) => ReadonlyArray<TecPnum>,
      ) {
        return function (x: TecPnumAST): TecPnumAST {
          return TecPnumAST.make({
            tecPnums: f(x.tecPnums) as Array<TecPnum>,
          });
        };
      },
    },
  },
};
