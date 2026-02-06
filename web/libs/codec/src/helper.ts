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
export function everyCombination<T>(ts: RNE<RNE<T>>): RNE<RNE<T>> {
  const [head, tail] = E.Array.unprepend(ts);
  if (E.Array.isNonEmptyReadonlyArray(tail)) {
    return E.Array.flatMap(head, (h) =>
      E.Array.map(everyCombination(tail), (t) => E.Array.prepend(t, h)),
    );
  } else {
    return E.Array.map(head, (x) => [x]);
  }
}
export function iterateIndexCombo(
  enumAST: TE.TecEnumAST,
  indexTypeSet: RNE<TC.TecClassIndex>,
) {
  return function (combo: TN.IndexCombination) {
    return E.Either.gen(function* () {
      const seed = yield* E.pipe(
        combo,
        E.Array.map((x, idx): E.Either.Either<RNE<TE.TecEnumValue>, Error> => {
          if (x.tag === "TecNodeIndexWildcard") {
            return E.Either.gen(function* () {
              const indexType = indexTypeSet[idx];
              const enumDef = yield* E.pipe(
                enumAST.tecEnums,
                E.Array.findFirst(
                  (e) => (e.tecEnumName as string) === indexType,
                ),
                E.Either.fromOption(
                  () => new Error("Failed to find enum: " + indexType),
                ),
              );
              return enumDef.tecEnumValues;
            });
          } else {
            return E.Either.right([x.contents]);
          }
        }),
        E.Either.all,
      );
      return everyCombination(seed);
    });
  };
}
