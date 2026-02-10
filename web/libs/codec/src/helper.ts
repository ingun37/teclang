import * as E from "effect";
import * as TC from "./TecClass.js";
import * as TE from "./TecEnum.js";
import * as TN from "./TecNode.js";
type RNE<T> = E.Array.NonEmptyReadonlyArray<T>;
type EV = TE.TecEnumValue;
type C = RNE<EV>;
export function makeFindEnum(enumAst: TE.TecEnumAST) {
  function option(name: string): E.Option.Option<TE.TecEnum> {
    return E.Array.findFirst(
      enumAst.tecEnums,
      (e) => (e.tecEnumName as string) === name,
    );
  }
  function either(name: string): E.Either.Either<TE.TecEnum, Error> {
    return E.pipe(
      option(name),
      E.Either.fromOption(() => new Error("Failed to find: " + name)),
    );
  }
  function force(name: string): TE.TecEnum {
    return E.Effect.runSync(either(name));
  }
  return {
    option,
    either,
    force,
  };
}
export function makeFindClass(enumAst: TC.TecClassAST) {
  function option(name: string): E.Option.Option<TC.TecClass> {
    return E.Array.findFirst(
      enumAst.tecClasses,
      (e) => (e.tecClassName as string) === name,
    );
  }
  function either(name: string): E.Either.Either<TC.TecClass, Error> {
    return E.pipe(
      option(name),
      E.Either.fromOption(() => new Error("Failed to find: " + name)),
    );
  }
  function force(name: string): TC.TecClass {
    return E.Effect.runSync(either(name));
  }
  return {
    option,
    either,
    force,
  };
}
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
  const findEnum = makeFindEnum(enumAST);
  return function (combo: TN.IndexCombination): E.Either.Either<RNE<C>, Error> {
    return E.Either.gen(function* () {
      const seed = yield* E.pipe(
        combo,
        E.Array.map((x, idx): E.Either.Either<RNE<TE.TecEnumValue>, Error> => {
          if (x.tag === "TecNodeIndexWildcard") {
            return E.Either.gen(function* () {
              const enumDef = yield* findEnum.either(indexTypeSet[idx]);
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
  if (at === "Number") return "REAL";
  return "TEXT";
}
function makeAttributeName(sig: TC.TecSignature) {
  return function (at: TC.TecClassAttribute) {
    return `a_${sig.attributeTypeSet.indexOf(at)}_${at.toLowerCase()}`;
  };
}
function classToSql(enumAst: TE.TecEnumAST) {
  const findEnum = makeFindEnum(enumAst);
  return function (tc: TC.TecClass) {
    const [primitiveAttributes, enumAttributes] = E.Array.partition(
      tc.tecSignature.attributeTypeSet,
      (at) => E.Option.isSome(findEnum.option(at)),
    );
    const attributeName = makeAttributeName(tc.tecSignature);

    const enumAttributeStatements = enumAttributes.map(
      (a) => `${attributeName(a)} TEXT NOT NULL`,
    );
    const enumAttributeChecks = enumAttributes.map(
      (it) => `FOREIGN KEY (${attributeName(it)}) REFERENCES ${it}(id)`,
    );
    const primitiveAttributeStatements = primitiveAttributes.map(
      (a) => `${attributeName(a)} ${attributeTypeToSqlType(a)} NOT NULL`,
    );

    if (tc.tecSignature.indexTypeSet.length === 1) {
      const te = findEnum.force(tc.tecSignature.indexTypeSet[0]);
      const idDecl = `id TEXT PRIMARY KEY CHECK (id IN (${te.tecEnumValues.map((v) => `'${v}'`).join(", ")}))`;

      const statements = [idDecl].concat(
        enumAttributeStatements,
        primitiveAttributeStatements,
        enumAttributeChecks,
      );
      return `
CREATE TABLE ${tc.tecClassName} (
${statements.filter((x) => x !== "").join(",\n")}
);`;
    } else {
      const primaryDecl = `PRIMARY KEY (${tc.tecSignature.indexTypeSet.map((x) => x.toLowerCase()).join(", ")})`;
      const compositeKeyStatements = tc.tecSignature.indexTypeSet.map(
        (it) => `${it.toLowerCase()} TEXT NOT NULL`,
      );
      const foreignKeyStatements = tc.tecSignature.indexTypeSet.map(
        (it) => `FOREIGN KEY (${it.toLowerCase()}) REFERENCES ${it}(id)`,
      );

      const statements = [
        compositeKeyStatements,
        enumAttributeStatements,
        primitiveAttributeStatements,
        [primaryDecl],
        foreignKeyStatements,
        enumAttributeChecks,
      ].flat();
      return `
CREATE TABLE ${tc.tecClassName} (
${statements.filter((x) => x !== "").join(",\n")}
);`;
    }
  };
}
function tecNodeAttributeToSql() {
  return function (tecNodeAttribute: TN.TecNodeAttribute) {
    switch (tecNodeAttribute.tag) {
      case "TecNodeConAttribute":
        return `'${tecNodeAttribute.contents}'`;
      case "TecNodeFracAttribute":
        return `${tecNodeAttribute.contents.numerator / tecNodeAttribute.contents.denominator}`;
      case "TecNodeIntAttribute":
        return tecNodeAttribute.contents.toString();
      case "TecNodeTextAttribute":
        return `'${tecNodeAttribute.contents}'`;
    }
  };
}
function tecNodeToSql(enumAst: TE.TecEnumAST, tecClass: TC.TecClass) {
  const iterator = iterateIndexCombo(
    enumAst,
    tecClass.tecSignature.indexTypeSet,
  );
  return function (tecNode: TN.TecNode) {
    return E.Effect.runSync(
      E.Either.gen(function* () {
        const combos_ = yield* iterator(tecNode.indexCombination);
        const combos = combos_.map((xs) =>
          xs
            .map((x) => `'${x}'`)
            .concat(tecNode.tecNodeAttributes.map(tecNodeAttributeToSql())),
        );
        return combos.map((combo) => `(${combo.join(", ")})`).join(",\n");
      }),
    );
  };
}
function nodeSetToSql(enumAst: TE.TecEnumAST, classAst: TC.TecClassAST) {
  const findClass = makeFindClass(classAst).force;
  return function (tn: TN.TecNodeSet) {
    const tc = findClass(tn.tecNodeClass);
    const attributeName = makeAttributeName(tc.tecSignature);
    const orderedIndexTypes =
      tc.tecSignature.indexTypeSet.length === 1
        ? ["id"]
        : tc.tecSignature.indexTypeSet.map((x) => x.toLowerCase());
    const orderedAttributeTypes =
      tc.tecSignature.attributeTypeSet.map(attributeName);
    const orderedTypes = orderedIndexTypes.concat(orderedAttributeTypes);

    const values = tn.tecNodeSet.map(tecNodeToSql(enumAst, tc)).join(",\n");

    return `INSERT INTO ${tn.tecNodeClass} (${orderedTypes.join(", ")}) VALUES ${values};`;
  };
}

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
${classAst.tecClasses.map(classToSql(enumAst)).join("\n")}
${nodeAst.tecNodeSets.map(nodeSetToSql(enumAst, classAst)).join("\n")}
`;
}
