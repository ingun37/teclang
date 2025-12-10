import { ParseResult, pipe, Schema as S } from "effect";

export enum TecSide {
  Front,
  Back,
  Left,
  Right,
}
const __TecSide = S.Enums(TecSide);

const UnitCon = S.Struct({
  tag: S.Literal("TecType"),
  typeName: S.String,
  parameters: S.Array(S.Any).pipe(S.itemsCount(0)),
});
type UnitCon = typeof UnitCon.Type;
export const _TecSide: S.Schema<TecSide, any> = S.transformOrFail(
  UnitCon,
  __TecSide,
  {
    strict: false,
    decode(x: UnitCon, _, ast) {
      return pipe(
        TecSide[x.typeName],
        S.decodeUnknownOption(__TecSide),
        ParseResult.fromOption(
          () => new ParseResult.Type(ast, x, "Failed to encode tecside"),
        ),
      );
    },
    encode(x) {
      return ParseResult.succeed({
        tag: "TecType",
        typeName: TecSide[x],
        parameters: [],
      } as UnitCon);
    },
  },
);
