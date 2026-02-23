import { Schema as S } from "effect";
import * as E from "effect";
import * as TecClass from "./TecClass.js";
import * as TecEnum from "./TecEnum.js";

export const TecNodeIntAttribute = S.Struct({
  tag: S.tag("TecNodeIntAttribute"),
  contents: S.Number.pipe(S.int()),
});

export const TecNodeFracAttribute = S.Struct({
  tag: S.tag("TecNodeFracAttribute"),
  contents: S.Struct({
    numerator: S.Number.pipe(S.int()),
    denominator: S.Number.pipe(S.int()),
  }),
});

export const TecNodeTextAttribute = S.Struct({
  tag: S.tag("TecNodeTextAttribute"),
  contents: S.String,
});

export const TecNodeConAttribute = S.Struct({
  tag: S.tag("TecNodeConAttribute"),
  contents: S.String,
});

const TecNodeAttribute = S.Union(
  TecNodeIntAttribute,
  TecNodeFracAttribute,
  TecNodeTextAttribute,
  TecNodeConAttribute,
);

export type TecNodeAttribute = typeof TecNodeAttribute.Type;
export const TecNodeIndexWildcard = S.Struct({
  tag: S.tag("TecNodeIndexWildcard"),
});
export const TecNodeIndexInst = S.Struct({
  tag: S.tag("TecNodeIndexInst"),
  contents: TecEnum.TecEnumValue,
});
export type TecNodeIndexWildcard = typeof TecNodeIndexWildcard.Type;
export type TecNodeIndexInst = typeof TecNodeIndexInst.Type;
const TecNodeIndex = S.Union(TecNodeIndexWildcard, TecNodeIndexInst);
export const tecNodeIndexEquivalence = S.equivalence(TecNodeIndex);
export type TecNodeIndex = typeof TecNodeIndex.Type;
export type TecNodeIndexSet = E.Array.NonEmptyReadonlyArray<TecNodeIndex>;
const IndexCombination = S.NonEmptyArray(TecNodeIndex);
export type IndexCombination = typeof IndexCombination.Type;
export const TecNode = S.Struct({
  indexCombination: IndexCombination,
  tecNodeAttributes: S.Array(TecNodeAttribute),
});
export type TecNode = typeof TecNode.Type;

export const TecNodeSet = S.Struct({
  tecNodeClass: TecClass.TecClassName,
  tecNodeSet: S.NonEmptyArray(TecNode),
});
export type TecNodeSet = typeof TecNodeSet.Type;
type RNE<T> = E.Array.NonEmptyReadonlyArray<T>;
export const lens = {
  tecNodeSet: {
    over: {
      tecNodeSet(f: (tecNodeSet: RNE<TecNode>) => RNE<TecNode>) {
        return function (tecNodeSet: TecNodeSet): TecNodeSet {
          return TecNodeSet.make({
            tecNodeClass: tecNodeSet.tecNodeClass,
            tecNodeSet: f(tecNodeSet.tecNodeSet),
          });
        };
      },
    },
  },
};

export const TecNodeAST = S.Struct({ tecNodeSets: S.Array(TecNodeSet) });
export type TecNodeAST = typeof TecNodeAST.Type;
