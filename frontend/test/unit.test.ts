import { expect, test } from "vitest";
import { Array } from "effect";
import { combination, transpose } from "../src/functions";

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
