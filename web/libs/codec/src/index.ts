import * as TecType from "./TecType.js";
import { Schema as S } from "effect";

export * as TecType from "./TecType.js";

export function tecTypeToJson(decoded: TecType.TecType): any {
  return S.encodeUnknownSync(TecType.TecType)(decoded);
}
