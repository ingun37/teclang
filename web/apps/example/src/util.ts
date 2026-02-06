import * as C from "codec";
import * as E from "effect";
export function tecNodeIndexToStr(x: C.TecNode.TecNodeIndex): string {
  return x.tag === "TecNodeIndexWildcard" ? "_" : x.contents;
}

export function createRandomAttributeValue(
  tecClass: C.TecClass.TecClass,
  tecEnumAst: C.TecEnum.TecEnumAST,
  comb: readonly C.TecNode.TecNodeIndex[],
) {
  return function (
    at: C.TecClass.TecClassAttribute,
  ): C.TecNode.TecNodeAttribute {
    switch (at) {
      case "Color":
        const colors = ["#FF5733", "#33FF57", "#3357FF", "#F333FF", "#FFD700"];
        return C.TecNode.TecNodeTextAttribute.make({
          contents: colors[Math.floor(Math.random() * colors.length)]!,
        });
      case "Number":
        const randomValue = Math.random() * 30 + 10;
        const denominator = 10;
        const numerator = Math.round(randomValue * denominator);
        return C.TecNode.TecNodeFracAttribute.make({
          contents: {
            numerator,
            denominator,
          },
        });
      case "String":
        const randomName = [
          "Lion",
          "Tiger",
          "Bear",
          "Elephant",
          "Giraffe",
          "Zebra",
          "Panda",
          "Koala",
          "Kangaroo",
          "Penguin",
        ][Math.floor(Math.random() * 10)]!;
        return C.TecNode.TecNodeTextAttribute.make({
          contents: `"${randomName!}"`,
        });
      case "Image":
        return C.TecNode.TecNodeTextAttribute.make({
          contents: `/${tecClass.tecClassName}/${comb
            .map(tecNodeIndexToStr)
            .map((x) => x.toLowerCase())
            .join("-")}.png`,
        });
      default:
        return E.pipe(
          tecEnumAst.tecEnums,
          E.Array.findFirst((te) => (te.tecEnumName as string) === at),
          E.Option.map((te) => {
            return C.TecNode.TecNodeConAttribute.make({
              contents:
                te.tecEnumValues[
                  Math.floor(Math.random() * te.tecEnumValues.length)
                ]!,
            });
          }),
          E.Option.getOrElse(() =>
            C.TecNode.TecNodeConAttribute.make({
              contents: "Unknown",
            }),
          ),
        );
    }
  };
}
