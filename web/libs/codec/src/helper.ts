import * as E from "effect";
import * as TC from "./TecClass.js";
import * as TE from "./TecEnum.js";
import * as TN from "./TecNode.js";
type RNE<T> = E.Array.NonEmptyReadonlyArray<T>;
type EV = TE.TecEnumValue;
type C = TN.IndexCombination;
function recur(enums: RNE<TE.TecEnum>): RNE<C> {
  const [x, xs] = E.Array.unprepend(enums);
  if (E.Array.isNonEmptyReadonlyArray(xs)) {
    const tails: RNE<C> = recur(xs);
    return E.Array.flatMap(
      x.tecEnumValues,
      (ev: EV): RNE<C> => E.Array.map(tails, (tail: C): C => [ev, ...tail]),
    );
  } else {
    return E.Array.map(x.tecEnumValues, (ev: EV): C => [ev]);
  }
}

export function iterateIndexSet(enumAST: TE.TecEnumAST) {
  return function (
    tc: TC.TecClass,
  ): E.Either.Either<RNE<TN.IndexCombination>, Error> {
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
    );
  };
}
