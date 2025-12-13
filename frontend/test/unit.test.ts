import { expect, test } from "vitest";
import { Array, pipe } from "effect";
import { combination, transpose } from "../src/functions";
import { nodeAttributesToQuery } from "../src/transformers";
import { TecInt, TecList, TecQuery, TecStr, TecType } from "../src/schema/TecAstSchema";
import { NodeAttributes, NodeAttributesOrder } from "../src/graphdb";

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
  const single = () =>
    nodeAttributesToQuery([[{ typeName: "A", ids: [0], meta: {} }]]);

  expect(single).toThrowError();

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
test("order", () => {
  const answer = [
    [
      { typeName: "Colorway", ids: [0], meta: {} },
      { typeName: "Fabric", ids: ["A"], meta: {} },
      { typeName: "Pantone", ids: ["white"], meta: { color: "#ffffff" } },
    ],
    [
      { typeName: "Colorway", ids: [0], meta: {} },
      { typeName: "Render", ids: [0, "Front"], meta: {} },
    ],
    [
      { typeName: "Colorway", ids: [1], meta: {} },
      { typeName: "Fabric", ids: ["A"], meta: {} },
      { typeName: "Pantone", ids: ["black"], meta: { color: "#000000" } },
    ],
  ];
  const input: NodeAttributes[][] = [
    [
      { typeName: "Colorway", ids: [0], meta: {} },
      { typeName: "Fabric", ids: ["A"], meta: {} },
      { typeName: "Pantone", ids: ["white"], meta: { color: "#ffffff" } },
    ],
    [
      { typeName: "Colorway", ids: [1], meta: {} },
      { typeName: "Fabric", ids: ["A"], meta: {} },
      { typeName: "Pantone", ids: ["black"], meta: { color: "#000000" } },
    ],
    [
      { typeName: "Colorway", ids: [0], meta: {} },
      { typeName: "Render", ids: [0, "Front"], meta: {} },
    ],
  ];
  const ordered = pipe(input, Array.sort(Array.getOrder(NodeAttributesOrder)));
  expect(ordered).toStrictEqual(answer);
});
// [[{"typeName":"Colorway","ids":[0],"meta":{}},{"typeName":"Fabric","ids":["A"],"meta":{}},{"typeName":"Pantone","ids":["white"],"meta":{"color":"#ffffff"}}],[{"typeName":"Colorway","ids":[1],"meta":{}},{"typeName":"Fabric","ids":["A"],"meta":{}},{"typeName":"Pantone","ids":["black"],"meta":{"color":"#000000"}}],[{"typeName":"Colorway","ids":[0],"meta":{}},{"typeName":"Render","ids":[0,"Front"],"meta":{}}]]
// [[{"typeName":"Colorway","ids":[0],"meta":{}},{"typeName":"Fabric","ids":["A"],"meta":{}},{"typeName":"Pantone","ids":["white"],"meta":{"color":"#ffffff"}}],[{"typeName":"Colorway","ids":[0],"meta":{}},{"typeName":"Render","ids":[0,"Front"],"meta":{}}],[{"typeName":"Colorway","ids":[1],"meta":{}},{"typeName":"Fabric","ids":["A"],"meta":{}},{"typeName":"Pantone","ids":["black"],"meta":{"color":"#000000"}}]]
