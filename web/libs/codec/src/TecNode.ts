import { Schema as S } from "effect";
import * as TecClass from "./TecClass.js";
import * as TecEnum from "./TecEnum.js";

const IndexCombination = S.NonEmptyArray(TecEnum.TecEnumValue);
export type IndexCombination = typeof IndexCombination.Type;
export const TecNode = S.Struct({
  indexCombination: IndexCombination,
  tecNodeAttributes: S.Array(S.Any),
});
export type TecNode = typeof TecNode.Type;

export const TecNodeSet = S.Struct({
  tecNodeClass: TecClass.TecClassName,
  tecNodeSet: S.Array(TecNode),
});
export type TecNodeSet = typeof TecNodeSet.Type;

export const TecNodeAST = S.Struct({ tecNodeSets: S.Array(TecNodeSet) });
export type TecNodeAST = typeof TecNodeAST.Type;
