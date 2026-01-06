import * as E from "effect";
import * as TC from "./TecClass.js";
import * as TE from "./TecEnum.js";
function* recur(enums: readonly TE.TecEnum[]): Generator<TE.TecEnumValue[]> {
  if (E.Array.isNonEmptyReadonlyArray(enums)) {
    const [x, xs] = E.Array.unprepend(enums);
    for (const v of x.tecEnumValues) {
      for (const tailV of recur(xs)) {
        yield [v, ...tailV];
      }
    }
  } else {
    yield [];
  }
}
export function iterateIndexSet(enumAST: TE.TecEnumAST) {
  return function (tc: TC.TecClass) {
    return E.pipe(
      tc.tecSignature.indexTypeSet,
      E.Array.map((t) =>
        E.Either.fromOption(
          E.Array.findFirst(
            enumAST.tecEnums,
            (e) => e.tecEnumName.toLowerCase() === t.toLowerCase(),
          ),
          () => new Error("Failed to find enum with the type"),
        ),
      ),
      E.Either.all,
      E.Either.map(recur),
      E.Either.map(E.Array.fromIterable),
    );
  };
}
