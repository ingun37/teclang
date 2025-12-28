import * as TecType from "./TecType.js";
import { Schema as S } from "effect";

export * as TecType from "./TecType.js";

export function uniqueTecTypeToJson(decoded: TecType.UniqueTecType): any {
  return S.encodeUnknownSync(TecType.UniqueTecTypeFromTecType)(decoded);
}

export function jsonToUniqueTecType(json: any): TecType.UniqueTecType {
  return S.decodeUnknownSync(TecType.UniqueTecTypeFromTecType)(json);
}
