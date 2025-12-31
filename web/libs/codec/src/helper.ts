import * as E from "effect";
import { Schema as S } from "effect";
export interface Branding<T extends S.Schema.Any> {
  (t: S.Schema.Encoded<T>): S.Schema.Type<T>;
}
export function commuteEncode<A, B, C, D extends S.Schema<B, A, never>>(
  encoder: S.Schema<B, C, never>,
  decoder: D,
) {
  return (x: A) => S.encodeEither(encoder)(S.decodeSync(decoder)(x));
}

export function commuteEncodeId<A, B>(sch: S.Schema<B, A>) {
  return (x: A) => S.encodeEither(sch)(S.decodeSync(sch)(x));
}
