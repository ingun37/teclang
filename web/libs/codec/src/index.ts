import * as TecType from "./TecType.js";
import { Schema as S } from "effect";

export * as TecType from "./TecType.js";

export function tecSchemaToJson(decoded: TecType.TecSchema): any {
  return S.encodeUnknownSync(TecType.TecSchemaFromTecType)(decoded);
}

export function jsonToTecSchema(json: any): TecType.TecSchema {
  return S.decodeUnknownSync(TecType.TecSchemaFromTecType)(json);
}
