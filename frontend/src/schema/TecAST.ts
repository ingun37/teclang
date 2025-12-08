import { Schema } from "effect";

const IndexN = Schema.Struct({
  tag: Schema.tag("IndexN"),
  number: Schema.Number,
});
const IndexS = Schema.Struct({
  tag: Schema.tag("IndexS"),
  name: Schema.String,
});
const IndexU = Schema.Struct({
  tag: Schema.tag("IndexU"),
});
const Index = Schema.Union(IndexN, IndexS, IndexU);
const TecType = Schema.Struct({
  tag: Schema.tag("TecType"),
  typeName: Schema.String,
  index: Index,
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
