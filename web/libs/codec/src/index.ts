import * as TecEnum from "./TecEnum.js";
import * as TecClass from "./TecClass.js";
import { Schema as S } from "effect";

export * as TecEnum from "./TecEnum.js";
export * as TecClass from "./TecClass.js";

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
