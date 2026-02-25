import * as TecEnum from "./TecEnum.js";
import * as TecClass from "./TecClass.js";
import * as TecNode from "./TecNode.js";
import * as TecQuery from "./TecQuery.js";
import * as TecPnum from "./TecPnum.js";
import { Schema as S } from "effect";

export * as TecEnum from "./TecEnum.js";
export * as TecClass from "./TecClass.js";
export * as TecNode from "./TecNode.js";
export * as TecQuery from "./TecQuery.js";
export * as TecPnum from "./TecPnum.js";
export * as help from "./helper.js";

export function jsonToTecEnumAST(json: any): TecEnum.TecEnumAST {
  return S.decodeUnknownSync(TecEnum.TecEnumAST)(json);
}
export function tecEnumASTToJson(decoded: TecEnum.TecEnumAST): any {
  return S.encodeUnknownSync(TecEnum.TecEnumAST)(decoded);
}
export function jsonToTecClassAST(json: any): TecClass.TecClassAST {
  return S.decodeUnknownSync(TecClass.TecClassAST)(json);
}
export function tecClassASTToJson(decoded: TecClass.TecClassAST): any {
  return S.encodeUnknownSync(TecClass.TecClassAST)(decoded);
}

export function jsonToTecNodeAST(json: any): TecNode.TecNodeAST {
  return S.decodeUnknownSync(TecNode.TecNodeAST)(json);
}
export function tecNodeASTToJson(decoded: TecNode.TecNodeAST): any {
  return S.encodeUnknownSync(TecNode.TecNodeAST)(decoded);
}

export function jsonToTecQueryAST(json: any): TecQuery.TecQuery {
  return S.decodeUnknownSync(TecQuery.TecQuery)(json);
}
export function tecQueryASTToJson(decoded: TecQuery.TecQuery): any {
  return S.encodeUnknownSync(TecQuery.TecQuery)(decoded);
}

export function jsonToTecPnumAST(json: any): TecPnum.TecPnumAST {
  return S.decodeUnknownSync(TecPnum.TecPnumAST)(json);
}
export function tecPnumASTToJson(decoded: TecPnum.TecPnumAST): any {
  return S.encodeUnknownSync(TecPnum.TecPnumAST)(decoded);
}
