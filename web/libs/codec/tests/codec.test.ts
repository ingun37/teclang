import { expect, test } from "vitest";
import * as fs from "node:fs";
import { Schema as S } from "effect";
import * as lib from "../src/index.js";
test("parse TecType haskell test log", () => {
  const content = fs.readFileSync(
    "/Users/ingun/projects/teclang/wasm/out-type.log",
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
    const decoded = S.decodeUnknownSync(lib.TecType.TecType)(jsonObject);
    const encoded = S.encodeUnknownSync(lib.TecType.TecType)(decoded);
    expect(encoded).toStrictEqual(jsonObject);
  }
});

test("enum from sum", () => {
  const e = S.decodeUnknownSync(lib.TecType.TecEnumFromSum)({
    tecTypeName: "Foo",
    classes: [
      { className: "A", parameterTypes: [] },
      { className: "B", parameterTypes: [] },
    ],
  });

  expect(e).toStrictEqual({ tecTypeName: "Foo", values: ["A", "B"] });
});

test("unique tec type from tec type", () => {
  const u = S.decodeUnknownSync(lib.TecType.UniqueTecTypeFromTecType)({
    sumTypes: [
      {
        tecTypeName: "Side",
        classes: [
          { className: "Front", parameterTypes: [] },
          { className: "Back", parameterTypes: [] },
        ],
      },
      {
        tecTypeName: "TecType",
        classes: [{ className: "Render", parameterTypes: ["Side", "String"] }],
      },
    ],
  });

  expect(u).toStrictEqual({
    tecEnums: [{ tecTypeName: "Side", values: ["Front", "Back"] }],
    tecType: {
      tecTypeName: "TecType",
      classes: [{ className: "Render", parameterTypes: ["Side", "String"] }],
    },
  });
});

test("unique tec type from tec type fail", () => {
  const f = () =>
    S.decodeUnknownSync(lib.TecType.UniqueTecTypeFromTecType)({
      sumTypes: [
        {
          tecTypeName: "Side",
          classes: [
            { className: "Front", parameterTypes: ["Ha!"] },
            { className: "Back", parameterTypes: [] },
          ],
        },
        {
          tecTypeName: "TecType",
          classes: [
            { className: "Render", parameterTypes: ["Side", "String"] },
          ],
        },
      ],
    });

  expect(f).toThrowError("Class cannot have parameter types in an enum");
});
