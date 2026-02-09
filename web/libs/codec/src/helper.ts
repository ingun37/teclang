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

function attributeTypeToSqlType(at: TC.TecClassAttribute) {
  // if (at === "String") return "TEXT";
  // if (at === "number") return "INTEGER";
  // if (at === "boolean") return "BOOLEAN";
  return "TEXT";
}
function classToSql(enumAst: TE.TecEnumAST) {
  return function (tc: TC.TecClass) {
    const [primitiveAttributes, enumAttributes] = E.Array.partition(
      tc.tecSignature.attributeTypeSet,
      (at) => enumAst.tecEnums.some((e) => (e.tecEnumName as string) === at),
    );
    const attributeName = (at: TC.TecClassAttribute) =>
      `a_${tc.tecSignature.attributeTypeSet.indexOf(at)}_${at.toLowerCase()}`;

    const enumAttributeStatements = enumAttributes
      .map((a) => `${attributeName(a)} TEXT NOT NULL`)
      .join(",\n");

    const enumAttributeChecks = enumAttributes
      .map((it) => `FOREIGN KEY (${attributeName(it)}) REFERENCES ${it}(id)`)
      .join(",\n");
    const primitiveAttributeStatements = primitiveAttributes
      .map((a) => `${attributeName(a)} ${attributeTypeToSqlType(a)} NOT NULL`)
      .join(",\n");

    if (tc.tecSignature.indexTypeSet.length === 1) {
      const te = enumAst.tecEnums.find(
        (e) => (e.tecEnumName as string) === tc.tecSignature.indexTypeSet[0],
      )!;
      return `
CREATE TABLE ${tc.tecClassName} (
id TEXT PRIMARY KEY CHECK (id IN (${te.tecEnumValues.map((v) => `'${v}'`).join(", ")})),
${[enumAttributeStatements, primitiveAttributeStatements, enumAttributeChecks].filter((x) => x !== "").join(",\n")}
);`;
    } else {
      const compositeKeyStatements = tc.tecSignature.indexTypeSet
        .map((it) => `${it.toLowerCase()} TEXT NOT NULL`)
        .join(",\n");
      const foreignKeyStatements = tc.tecSignature.indexTypeSet
        .map((it) => `FOREIGN KEY (${it.toLowerCase()}) REFERENCES ${it}(id)`)
        .join(",\n");

      return `
CREATE TABLE ${tc.tecClassName} (
${[compositeKeyStatements, enumAttributeStatements, primitiveAttributeStatements].filter((x) => x !== "").join(",\n")},
PRIMARY KEY (${tc.tecSignature.indexTypeSet.map((x) => x.toLowerCase()).join(", ")}),
${[foreignKeyStatements, enumAttributeChecks].filter((x) => x !== "").join(",\n")}
);`;
    }
  };
}

function nodeToSql(tn: TN.TecNodeSet) {}

export function generateSqlSchema(
  enumAst: TE.TecEnumAST,
  classAst: TC.TecClassAST,
  nodeAst: TN.TecNodeAST,
) {
  const realEnums = enumAst.tecEnums.filter((te) =>
    classAst.tecClasses.every(
      (tc) =>
        tc.tecSignature.indexTypeSet.length > 1 ||
        (tc.tecSignature.indexTypeSet[0] as string) !==
          (te.tecEnumName as string),
    ),
  );
  return `
${realEnums.map(tecEnumToSql).join("\n")}
${classAst.tecClasses.map(classToSql(enumAst)).join("\n")}`;
}
