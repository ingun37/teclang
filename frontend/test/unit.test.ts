import { expect, test } from "vitest";
import { Array } from "effect";
import { combination } from "../src/functions";

test("adds 1 + 2 to equal 3", () => {
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
