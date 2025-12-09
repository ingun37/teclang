import { Schema } from "effect";

// enum Side {
//   Front,
//   Back,
//   Left,
//   Right,
// }
// const TecEnumSide = Schema.Struct({
//   label: Schema.Enums(Side),
//   typeName: Schema.Literal("Side"),
//   value: Schema.Number,
// });
//
// const TecEnumInt = Schema.Struct({
//   label: Schema.Literal(""),
//   typeName: Schema.Literal(""),
//   value: Schema.Number,
// });
// const TecEnum = Schema.Union(TecEnumSide, TecEnumInt);

const TecEnum = Schema.Struct({
  label: Schema.String,
  typeName: Schema.String,
  value: Schema.Number,
});
const IndexS = Schema.Struct({
  tag: Schema.tag("IndexS"),
  name: Schema.String,
});
const IndexU = Schema.Struct({
  tag: Schema.tag("IndexU"),
});
const indexE = Schema.Struct({
  tag: Schema.tag("IndexE"),
  contents: TecEnum,
});
const IndexR = Schema.Struct({
  tag: Schema.tag("IndexR"),
  from: TecEnum,
  to: Schema.NullOr(TecEnum),
});
const Index = Schema.Union(IndexS, IndexU, indexE, IndexR);
const TecType = Schema.Struct({
  tag: Schema.tag("TecType"),
  typeName: Schema.String,
  index: Index,
  index1: Schema.NullOr(Index),
});
export type TecType = typeof TecType.Type;
export type TecAST = TecLayout | TecQuery | TecType;
export interface TecLayout {
  readonly tag: "TecLayout";
  readonly typeName: string;
  readonly children: readonly TecAST[];
}
export interface TecQuery {
  readonly tag: "TecQuery";
  readonly op: string;
  readonly left: TecAST;
  readonly right: TecAST;
}
const TecLayout = Schema.Struct({
  tag: Schema.tag("TecLayout"),
  typeName: Schema.String,
  children: Schema.Array(Schema.suspend((): Schema.Schema<TecAST> => TecAST)),
});
const TecQuery = Schema.Struct({
  tag: Schema.tag("TecQuery"),
  op: Schema.String,
  left: Schema.suspend((): Schema.Schema<TecAST> => TecAST),
  right: Schema.suspend((): Schema.Schema<TecAST> => TecAST),
});
const TecAST = Schema.Union(
  Schema.suspend((): Schema.Schema<TecLayout> => TecLayout),
  Schema.suspend((): Schema.Schema<TecQuery> => TecQuery),
  TecType,
);

export function decodeTecAST(o: any): TecAST {
  return Schema.decodeUnknownSync(TecAST)(o);
}
/*

{"tag":"TecType","typeName":"Colorway","index":{"tag":"IndexN","number":0}}
 */
