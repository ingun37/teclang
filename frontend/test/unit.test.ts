import { expect, test } from "vitest";
import { Array } from "effect";
import { combination, transpose } from "../src/functions";
import { nodeAttributesToQuery } from "../src/transformers";
import {
  TecInt,
  TecList,
  TecQuery,
  TecStr,
  TecType,
} from "../src/schema/TecAstSchema";

test("combination", () => {
  expect(Array.fromIterable(combination([1], 1))).toStrictEqual([[1]]);

  expect(Array.fromIterable(combination([1, 2], 1))).toStrictEqual([[1], [2]]);
  expect(Array.fromIterable(combination([1, 2, 3], 3))).toStrictEqual([
    [1, 2, 3],
  ]);

  expect(Array.fromIterable(combination([1, 2, 3, 4], 3))).toStrictEqual([
    [1, 2, 3],
    [1, 2, 4],
    [1, 3, 4],
    [2, 3, 4],
  ]);
});

test("transpose", () => {
  expect(transpose([[1]])).toStrictEqual([[1]]);
  expect(transpose([[1], [2], [3]])).toStrictEqual([[1, 2, 3]]);
  expect(transpose(transpose([[1], [2], [3]]))).toStrictEqual([[1], [2], [3]]);
  expect(
    transpose([
      [1, 2, 3],
      [4, 5, 6],
    ]),
  ).toStrictEqual([
    [1, 4],
    [2, 5],
    [3, 6],
  ]);
  expect(
    transpose(
      transpose([
        [1, 2, 3],
        [4, 5, 6],
      ]),
    ),
  ).toStrictEqual([
    [1, 2, 3],
    [4, 5, 6],
  ]);
});

test("transformer", () => {
  const query = nodeAttributesToQuery([
    [
      { typeName: "A", ids: [0], meta: {} },
      { typeName: "B", ids: ["a", 0x0010], meta: {} },
    ],
    [
      { typeName: "A", ids: [1], meta: {} },
      { typeName: "B", ids: ["b", 0x0011], meta: {} },
    ],
  ]);
  expect(query).toStrictEqual(
    TecQuery.make({
      op: ":-",
      left: TecType.make({
        typeName: "A",
        parameters: [
          TecList.make({
            list: [TecInt.make({ int: 0 }), TecInt.make({ int: 1 })],
          }),
        ],
      }),
      right: TecType.make({
        typeName: "B",
        parameters: [
          TecList.make({
            list: [TecStr.make({ str: "a" }), TecStr.make({ str: "b" })],
          }),
          TecList.make({
            list: [TecInt.make({ int: 0x0010 }), TecInt.make({ int: 0x0011 })],
          }),
        ],
      }),
    }),
  );
});
