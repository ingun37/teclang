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

test("helper", () => {
  const enumAST = lib.TecEnum.TecEnumAST.make({
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

  const iter = (idxTypes: E.Array.NonEmptyReadonlyArray<string>) =>
    E.Effect.runSync(
      lib.help.iterateIndexSet(enumAST)(
        lib.TecClass.TecClass.make({
          tecClassName: lib.TecClass.TecClassName.make("Foo"),
          tecSignature: lib.TecClass.TecSignature.make({
            attributeTypeSet: [lib.TecClass.TecClassAttribute.make("Attrib")],
            indexTypeSet: E.Array.map(idxTypes, (it) =>
              lib.TecClass.TecClassIndex.make(it),
            ),
          }),
        }),
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
