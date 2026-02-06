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
const enumAST: lib.TecEnum.TecEnumAST = lib.TecEnum.TecEnumAST.make({
  tecEnums: [
    lib.TecEnum.TecEnum.make({
      tecEnumName: lib.TecEnum.TecEnumName.make("Letter"),
      tecEnumValues: "ab"
        .split("")
        .map((l) => lib.TecEnum.TecEnumValue.make(l)) as any,
    }),
    lib.TecEnum.TecEnum.make({
      tecEnumName: lib.TecEnum.TecEnumName.make("Number"),
      tecEnumValues: "12"
        .split("")
        .map((l) => lib.TecEnum.TecEnumValue.make(l)) as any,
    }),
    lib.TecEnum.TecEnum.make({
      tecEnumName: lib.TecEnum.TecEnumName.make("Index"),
      tecEnumValues: "ij"
        .split("")
        .map((l) => lib.TecEnum.TecEnumValue.make(l)) as any,
    }),
  ],
});

test("helper", () => {
  const iter = (idxTypes: E.Array.NonEmptyReadonlyArray<string>) =>
    E.Effect.runSync(
      lib.help.iterateIndexSet(enumAST)(
        E.Array.map(idxTypes, (it) => lib.TecClass.TecClassIndex.make(it)),
      ),
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

const indexSet: lib.TecClass.TecClassIndexTypeSet = E.Array.map(
  enumAST.tecEnums as E.Array.NonEmptyReadonlyArray<lib.TecEnum.TecEnum>,
  (edef) => lib.TecClass.TecClassIndex.make(edef.tecEnumName),
);

test("iterateIndexCombo", () => {
  const run = (x) =>
    E.Effect.runSync(lib.help.iterateIndexCombo(enumAST, indexSet)(x));
  expect(
    run([
      lib.TecNode.TecNodeIndexInst.make({
        contents: enumAST.tecEnums[0].tecEnumValues[0],
      }),
      lib.TecNode.TecNodeIndexInst.make({
        contents: enumAST.tecEnums[1].tecEnumValues[0],
      }),
      lib.TecNode.TecNodeIndexInst.make({
        contents: enumAST.tecEnums[2].tecEnumValues[0],
      }),
    ]),
  ).toStrictEqual([["a", "1", "i"]]);
  expect(
    run([
      lib.TecNode.TecNodeIndexWildcard.make({}),
      lib.TecNode.TecNodeIndexWildcard.make({}),
      lib.TecNode.TecNodeIndexWildcard.make({}),
    ]),
  ).toStrictEqual([
    ["a", "1", "i"],
    ["a", "1", "j"],
    ["a", "2", "i"],
    ["a", "2", "j"],
    ["b", "1", "i"],
    ["b", "1", "j"],
    ["b", "2", "i"],
    ["b", "2", "j"],
  ]);

  expect(
    run([
      lib.TecNode.TecNodeIndexInst.make({
        contents: enumAST.tecEnums[0].tecEnumValues[0],
      }),
      lib.TecNode.TecNodeIndexInst.make({
        contents: enumAST.tecEnums[1].tecEnumValues[0],
      }),
      lib.TecNode.TecNodeIndexWildcard.make({}),
    ]),
  ).toStrictEqual([
    ["a", "1", "i"],
    ["a", "1", "j"],
  ]);

  expect(
    run([
      lib.TecNode.TecNodeIndexInst.make({
        contents: enumAST.tecEnums[0].tecEnumValues[0],
      }),
      lib.TecNode.TecNodeIndexWildcard.make({}),
      lib.TecNode.TecNodeIndexInst.make({
        contents: enumAST.tecEnums[2].tecEnumValues[0],
      }),
    ]),
  ).toStrictEqual([
    ["a", "1", "i"],
    ["a", "2", "i"],
  ]);
});
