import { Schema as S } from "effect";
import * as E from "effect";
export const TecIndexValues = S.Struct({
  tag: S.tag("TecIndexValues"),
  tecIndexValues: S.NonEmptyArray(S.String),
});
export type TecIndexValues = typeof TecIndexValues.Type;
export const TecIndexRange = S.Struct({
  tag: S.tag("TecIndexRange"),
  tecIndexFrom: S.String,
  tecIndexTo: S.Null,
});
export type TecIndexRange = typeof TecIndexRange.Type;
export const TecIndexSeq = S.Union(TecIndexValues, TecIndexRange);
export type TecIndexSeq = typeof TecIndexSeq.Type;
export const TecQuery = S.Struct({
  tecQueryFunc: S.String,
  tecQueryIndexSeqs: S.NonEmptyArray(TecIndexSeq),
});
export type TecQuery = typeof TecQuery.Type;
