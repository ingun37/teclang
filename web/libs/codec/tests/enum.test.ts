import { expect, test } from "vitest";
import * as fs from "node:fs";
import * as lib from "../src/index.js";
import * as E from "effect";
test("parse TecEnum haskell test log", () => {
  const content = fs.readFileSync(
    "/Users/ingun/projects/teclang/wasm/out-enum.log",
    "utf-8",
  );
  const sections = content.split("---- Json encoded ----");

  for (let i = 1; i < sections.length; i++) {
    const section = sections[i];
    // Find the end of the JSON object. It starts with { and we want to find the matching } or the next ----
    const nextHeaderIndex = section.indexOf("----");
    const jsonStr =
      nextHeaderIndex === -1
        ? section.trim()
        : section.substring(0, nextHeaderIndex).trim();
    expect(jsonStr).not.toBeNull();
    const jsonObject = JSON.parse(jsonStr);
    const decoded = lib.jsonToTecEnumAST(jsonObject);
    const encoded = lib.tecEnumASTToJson(decoded);
    expect(encoded).toStrictEqual(jsonObject);
  }
});

test("parse TecClass haskell test log", () => {
  const content = fs.readFileSync(
    "/Users/ingun/projects/teclang/wasm/out-class.log",
    "utf-8",
  );
  const sections = content.split("---- Json encoded ----");

  for (let i = 1; i < sections.length; i++) {
    const section = sections[i];
    // Find the end of the JSON object. It starts with { and we want to find the matching } or the next ----
    const nextHeaderIndex = section.indexOf("----");
    const jsonStr =
      nextHeaderIndex === -1
        ? section.trim()
        : section.substring(0, nextHeaderIndex).trim();
    expect(jsonStr).not.toBeNull();
    const jsonObject = JSON.parse(jsonStr);
    const decoded = lib.jsonToTecClassAST(jsonObject);
    const encoded = lib.tecClassASTToJson(decoded);
    expect(encoded).toStrictEqual(jsonObject);
  }
});

test("parse TecNode haskell test log", () => {
  const content = fs.readFileSync(
    "/Users/ingun/projects/teclang/wasm/out-node.log",
    "utf-8",
  );
  const sections = content.split("---- Json encoded ----");

  for (let i = 1; i < sections.length; i++) {
    const section = sections[i];
    // Find the end of the JSON object. It starts with { and we want to find the matching } or the next ----
    const nextHeaderIndex = section.indexOf("----");
    const jsonStr =
      nextHeaderIndex === -1
        ? section.trim()
        : section.substring(0, nextHeaderIndex).trim();
    expect(jsonStr).not.toBeNull();
    const jsonObject = JSON.parse(jsonStr);
    const decoded = lib.jsonToTecNodeAST(jsonObject);
    const encoded = lib.tecNodeASTToJson(decoded);
    expect(encoded).toStrictEqual(jsonObject);
  }
});
function createSampleEnum([name, values]: [string, string]) {
  return lib.TecEnum.TecEnum.make({
    tecEnumName: lib.TecEnum.TecEnumName.make(name),
    tecEnumValues: values
      .split("")
      .map((l) => lib.TecEnum.TecEnumValue.make(l)) as any,
  });
}
function createSampleEnumAst(nameValuesPairs: [string, string][]) {
  return lib.TecEnum.TecEnumAST.make({
    tecEnums: nameValuesPairs.map(createSampleEnum),
  });
}
function createSampleIndexTypeSet(indices: RNE<string>) {
  return E.Array.map(indices, (i) => lib.TecClass.TecClassIndex.make(i));
}
type RNE<T> = E.Array.NonEmptyReadonlyArray<T>;
function createSampleClass([name, attribs, indices]: [
  string,
  RNE<string>,
  RNE<string>,
]) {
  return lib.TecClass.TecClass.make({
    tecClassName: lib.TecClass.TecClassName.make(name),
    tecSignature: lib.TecClass.TecSignature.make({
      attributeTypeSet: E.Array.map(attribs, (a) =>
        lib.TecClass.TecClassAttribute.make(a),
      ),
      indexTypeSet: createSampleIndexTypeSet(indices),
    }),
  });
}

const createSampleClassAst = (classes: [string, RNE<string>, RNE<string>][]) =>
  lib.TecClass.TecClassAST.make({
    tecClasses: classes.map(createSampleClass),
  });

function createSampleIndexSet(indices: RNE<string>) {
  return E.Array.map(indices, (i) =>
    i === "_"
      ? lib.TecNode.TecNodeIndexWildcard.make({})
      : lib.TecNode.TecNodeIndexInst.make({
          contents: lib.TecEnum.TecEnumValue.make(i),
        }),
  );
}
function createSampleNode([indices, attribs]: [RNE<string>, string[]]) {
  return lib.TecNode.TecNode.make({
    indexCombination: createSampleIndexSet(indices),
    tecNodeAttributes: attribs.map((a) =>
      lib.TecNode.TecNodeTextAttribute.make({ contents: a }),
    ),
  });
}

function createSampleNodeSet(
  className: string,
  nodes: RNE<[RNE<string>, string[]]>,
) {
  return lib.TecNode.TecNodeSet.make({
    tecNodeClass: lib.TecClass.TecClassName.make(className),
    tecNodeSet: E.Array.map(nodes, createSampleNode),
  });
}

test("helper", () => {
  const iter = (idxTypes: E.Array.NonEmptyReadonlyArray<string>) =>
    E.Effect.runSync(
      lib.help.iterateIndexSet(
        createSampleEnumAst([
          ["Letter", "ab"],
          ["Number", "12"],
          ["Index", "ij"],
        ]),
      )(E.Array.map(idxTypes, (it) => lib.TecClass.TecClassIndex.make(it))),
    );
  expect(iter(["Letter"])).toStrictEqual([["a"], ["b"]]);
  expect(iter(["Letter", "Number", "Index"])).toStrictEqual([
    ["a", "1", "i"],
    ["a", "1", "j"],
    ["a", "2", "i"],
    ["a", "2", "j"],
    ["b", "1", "i"],
    ["b", "1", "j"],
    ["b", "2", "i"],
    ["b", "2", "j"],
  ]);
});

test("everycombination", () => {
  expect(lib.help.everyCombination([[1], [2]])).toStrictEqual([[1, 2]]);
  expect(
    lib.help.everyCombination([
      [1, 2],
      [-1, -2],
    ]),
  ).toStrictEqual([
    [1, -1],
    [1, -2],
    [2, -1],
    [2, -2],
  ]);
});

test("iterateIndexCombo", () => {
  const run = (x: lib.TecNode.TecNodeIndexSet) =>
    E.Effect.runSync(
      lib.help
        .iterateIndexCombo(
          createSampleEnumAst([
            ["Letter", "ab"],
            ["Number", "12"],
            ["Index", "ij"],
          ]),
          createSampleIndexTypeSet(["Letter", "Number", "Index"]),
        )
        .either(x),
    );
  expect(run(createSampleIndexSet(["a", "1", "i"]))).toStrictEqual([
    ["a", "1", "i"],
  ]);
  expect(run(createSampleIndexSet(["_", "_", "_"]))).toStrictEqual([
    ["a", "1", "i"],
    ["a", "1", "j"],
    ["a", "2", "i"],
    ["a", "2", "j"],
    ["b", "1", "i"],
    ["b", "1", "j"],
    ["b", "2", "i"],
    ["b", "2", "j"],
  ]);

  expect(run(createSampleIndexSet(["a", "1", "_"]))).toStrictEqual([
    ["a", "1", "i"],
    ["a", "1", "j"],
  ]);

  expect(run(createSampleIndexSet(["a", "_", "i"]))).toStrictEqual([
    ["a", "1", "i"],
    ["a", "2", "i"],
  ]);
});
function pair<A, B>(a: A, b: B): [A, B] {
  return [a, b];
}
function createIndexEnumValueCombo(
  values: string,
): lib.help.IndexEnumValueCombo {
  const arr = values.split("").map((v) => lib.TecEnum.TecEnumValue.make(v));
  if (E.Array.isNonEmptyArray(arr)) {
    return arr;
  } else throw new Error("Invalid values");
}
test("iterate node set", () => {
  const enumAst = createSampleEnumAst([
    ["X", "12"],
    ["Y", "ij"],
  ]);
  const sampleClass = createSampleClass(["C", ["A"], ["X", "Y"]]);
  const iterator = lib.help.iterateNodesInNodeSet(enumAst, sampleClass);

  expect(
    iterator.force(
      createSampleNodeSet("C", [pair(["_", "_"], ["a"])]).tecNodeSet,
    ),
  ).toStrictEqual(
    [["1i", "1j", "2i", "2j"]].map(E.Array.map(createIndexEnumValueCombo)),
  );

  expect(
    iterator.force(
      createSampleNodeSet("C", [
        pair(["1", "_"], ["a"]),
        pair(["_", "_"], ["a"]),
      ]).tecNodeSet,
    ),
  ).toStrictEqual(
    [
      ["1i", "1j"],
      ["2i", "2j"],
    ].map(E.Array.map(createIndexEnumValueCombo)),
  );

  expect(
    iterator.force(
      createSampleNodeSet("C", [
        pair(["_", "_"], ["a"]),
        pair(["1", "_"], ["a"]),
      ]).tecNodeSet,
    ),
  ).toStrictEqual(
    [["1i", "1j", "2i", "2j"], []].map(E.Array.map(createIndexEnumValueCombo)),
  );

  expect(
    iterator.force(
      createSampleNodeSet("C", [
        pair(["2", "j"], ["a"]),
        pair(["1", "_"], ["a"]),
        pair(["2", "i"], ["a"]),
      ]).tecNodeSet,
    ),
  ).toStrictEqual(
    [["2j"], ["1i", "1j"], ["2i"]].map(E.Array.map(createIndexEnumValueCombo)),
  );
});
