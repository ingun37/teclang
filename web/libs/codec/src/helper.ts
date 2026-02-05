import * as E from "effect";
import * as TC from "./TecClass.js";
import * as TE from "./TecEnum.js";
import * as TN from "./TecNode.js";
type RNE<T> = E.Array.NonEmptyReadonlyArray<T>;
type EV = TE.TecEnumValue;
type C = RNE<TN.TecNodeIndexInst>;
function recur(enums: RNE<TE.TecEnum>): RNE<C> {
  const [x, xs] = E.Array.unprepend(enums);
  if (E.Array.isNonEmptyReadonlyArray(xs)) {
    const tails: RNE<C> = recur(xs);
    return E.Array.flatMap(
      x.tecEnumValues,
      (ev: EV): RNE<C> =>
        E.Array.map(
          tails,
          (tail: C): C =>
            E.Array.prepend(tail, TN.TecNodeIndexInst.make({ contents: ev })),
        ),
    );
  } else {
    return E.Array.map(
      x.tecEnumValues,
      (ev: EV): C => [TN.TecNodeIndexInst.make({ contents: ev })],
    );
  }
}

export function iterateIndexSet(enumAST: TE.TecEnumAST) {
  return function (
    indexTypeSet: RNE<TC.TecClassIndex>,
  ): E.Either.Either<RNE<C>, Error> {
    return E.pipe(
      indexTypeSet,
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
