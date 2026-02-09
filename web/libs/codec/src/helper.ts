import * as E from "effect";
import * as TC from "./TecClass.js";
import * as TE from "./TecEnum.js";
import * as TN from "./TecNode.js";
type RNE<T> = E.Array.NonEmptyReadonlyArray<T>;
type EV = TE.TecEnumValue;
type C = RNE<EV>;

export type IndexEnumValueCombo = C;
export const indexEnumValueComboEq: E.Equivalence.Equivalence<IndexEnumValueCombo> =
  E.Array.getEquivalence(E.Equivalence.string);
function recur(enums: RNE<TE.TecEnum>): RNE<C> {
  const [x, xs] = E.Array.unprepend(enums);
  if (E.Array.isNonEmptyReadonlyArray(xs)) {
    const tails: RNE<C> = recur(xs);
    return E.Array.flatMap(
      x.tecEnumValues,
      (ev: EV): RNE<C> =>
        E.Array.map(tails, (tail: C): C => E.Array.prepend(tail, ev)),
    );
  } else {
    return E.Array.map(x.tecEnumValues, (ev: EV): C => [ev]);
  }
}

export function findEnumOfIndexType(enumAST: TE.TecEnumAST) {
  return function (indexType: TC.TecClassIndex) {
    return E.pipe(
      enumAST.tecEnums,
      E.Array.findFirst((e) => (e.tecEnumName as string) === indexType),
    );
  };
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
  return function (combo: TN.IndexCombination): E.Either.Either<RNE<C>, Error> {
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
function tecEnumToSql(enumDef: TE.TecEnum) {
  return `
  CREATE TABLE ${enumDef.tecEnumName} (
    id TEXT PRIMARY KEY CHECK (id IN (${enumDef.tecEnumValues.map((v) => `'${v}'`).join(", ")}))
  );`;
}
type ECPair = [TE.TecEnum, TC.TecClass];

function enumClassToSql([te, tc]: ECPair) {
  return `
  CREATE TABLE ${te.tecEnumName} (
    id TEXT PRIMARY KEY CHECK (id IN (${te.tecEnumValues.map((v) => `'${v}'`).join(", ")})),
    ${tc.tecSignature.attributeTypeSet.map((a) => ` ${a.toLowerCase()} TEXT NOT NULL`).join(",\n")}
  );`;
}
function classToSql(tc: TC.TecClass) {
  return `
  CREATE TABLE ${tc.tecClassName} (
    ${tc.tecSignature.indexTypeSet.map((it) => `${it.toLowerCase()} TEXT NOT NULL`)},
    ${tc.tecSignature.attributeTypeSet.map((a) => ` ${a.toLowerCase()} TEXT NOT NULL`).join(",\n")},
    PRIMARY KEY (${tc.tecSignature.indexTypeSet.map((x) => x.toLowerCase()).join(", ")}),
    ${tc.tecSignature.indexTypeSet.map((it) => `    FOREIGN KEY (${it.toLowerCase()}) REFERENCES ${it}(id)`).join(",\n")}
  );`;
}
export function generateSqlSchema(
  enumAst: TE.TecEnumAST,
  classAst: TC.TecClassAST,
) {
  function pairIf(te: TE.TecEnum) {
    return function (tc: TC.TecClass): E.Option.Option<ECPair> {
      if (
        tc.tecSignature.indexTypeSet.length === 1 &&
        (tc.tecSignature.indexTypeSet[0] as string) === te.tecEnumName
      )
        return E.Option.some([te, tc] as ECPair);
      return E.Option.none();
    };
  }
  const [enums, enumClasses] = E.pipe(
    enumAst.tecEnums,
    E.Array.partitionMap((enumDef: TE.TecEnum) =>
      E.Either.fromOption(
        E.Array.findFirst(classAst.tecClasses, pairIf(enumDef)),
        () => enumDef,
      ),
    ),
  );
  const classes = classAst.tecClasses.filter((classDef) => {
    for (const [, c] of enumClasses) {
      if (c.tecClassName === classDef.tecClassName) return false;
    }
    return true;
  });
  return `
  ${enums.map(tecEnumToSql).join("\n")}
  ${enumClasses.map(enumClassToSql).join("\n")}
  ${classes.map(classToSql).join("\n")}
  `;
}
