import { expect, test } from "vitest";
import * as fs from "node:fs";
import { Schema as S } from "effect";
import * as E from "effect";
import * as lib from "../src/index.js";
test("parse TecType haskell test log", () => {
  const content = fs.readFileSync(
    "/Users/ingun/projects/teclang/wasm/out-type.log",
    "utf-8",
  );
  const sections = content.split("---- Json encoded ----");
  const jsonObjects = [];

  for (let i = 1; i < sections.length; i++) {
    const section = sections[i];
    // Find the end of the JSON object. It starts with { and we want to find the matching } or the next ----
    const nextHeaderIndex = section.indexOf("----");
    const jsonStr =
      nextHeaderIndex === -1
        ? section.trim()
        : section.substring(0, nextHeaderIndex).trim();
    if (jsonStr) {
      jsonObjects.push(JSON.parse(jsonStr));
    }
  }

  const result = E.pipe(
    jsonObjects,
    E.Array.map((x) => S.decodeUnknownEither(lib.TecType.TecType)(x)),
    E.Either.all,
    E.Either.mapLeft((x) => x.toString()),
  );

  expect(result._tag).toEqual("Right");
});
