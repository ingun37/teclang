<script setup lang="ts">
import * as C from "codec";
import * as E from "effect";
import { useAppStore } from "@/stores/app.ts";
const props = defineProps<{
  tecClassAst: C.TecClass.TecClassAST;
  tecEnumAst: C.TecEnum.TecEnumAST;
}>();

defineEmits<{
  (e: "update", value: C.TecNode.TecNodeAST): void;
}>();
const tecNodeAST = ref<C.TecNode.TecNodeAST>(
  C.TecNode.TecNodeAST.make({ tecNodeSets: [] }),
);

function lowerFirst(str: string): string {
  if (str.length === 0) return str;
  return str.charAt(0).toLowerCase() + str.slice(1);
}

const haskellCode = ref("");
function tecNodeIndexToStr(x: C.TecNode.TecNodeIndex): string {
  return x.tag === "TecNodeIndexWildcard" ? "_" : x.contents;
}
function createInitialHaskellCode() {
  const combinations = (x: C.TecClass.TecClassIndexTypeSet) =>
    E.pipe(
      C.help.iterateIndexSet(props.tecEnumAst)(x),
      E.Either.map(
        E.Array.map(
          E.Array.map((x) => C.TecNode.TecNodeIndexInst.make({ contents: x })),
        ),
      ),
    );

  const a = E.Effect.runSync(
    E.pipe(
      props.tecClassAst.tecClasses,
      E.Array.map((c) =>
        E.Either.gen(function* () {
          const useWildCard = c.tecSignature.indexTypeSet.length > 2;
          const indexTypeSet = useWildCard
            ? E.Array.dropRight(c.tecSignature.indexTypeSet, 1)
            : c.tecSignature.indexTypeSet;

          if (!E.Array.isNonEmptyReadonlyArray(indexTypeSet))
            throw new Error("Unknown error 0x0fe8a2");

          const _combs = yield* combinations(indexTypeSet);
          const combs = useWildCard
            ? E.Array.map(
                _combs,
                E.Array.append(C.TecNode.TecNodeIndexWildcard.make({})),
              )
            : _combs;
          const each = combs.map((comb) => {
            const sampleAttributes = c.tecSignature.attributeTypeSet.map(
              (at) => {
                switch (at) {
                  case "Color":
                    const colors = [
                      '"#FF5733"',
                      '"#33FF57"',
                      '"#3357FF"',
                      '"#F333FF"',
                      '"#FFD700"',
                    ];
                    return colors[Math.floor(Math.random() * colors.length)]!;
                  case "Number":
                    return (Math.random() * 30 + 10).toFixed(1);
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
                    return `"${randomName!}"`;
                  case "Image":
                    return `"/${c.tecClassName}/${comb
                      .map(tecNodeIndexToStr)
                      .map((x) => x.toLowerCase())
                      .join("-")}.png"`;
                  default:
                    return E.pipe(
                      props.tecEnumAst.tecEnums,
                      E.Array.findFirst(
                        (te) => (te.tecEnumName as string) === at,
                      ),
                      E.Option.map(
                        (te) =>
                          te.tecEnumValues[
                            Math.floor(Math.random() * te.tecEnumValues.length)
                          ],
                      ),
                      E.Option.getOrElse(() => "unknown"),
                    );
                }
              },
            );
            const xxx =
              sampleAttributes.length == 1
                ? sampleAttributes[0]
                : `(${sampleAttributes.join(", ")})`;
            return `${lowerFirst(c.tecClassName)} ${comb.map(tecNodeIndexToStr).join(" ")} = ${xxx}`;
          });
          return each.join("\n");
        }),
      ),
      E.Either.all,
    ),
  );
  return a.join("\n");
}

function fillSampleCode() {
  haskellCode.value = createInitialHaskellCode();
}
async function updateUI() {
  tecNodeAST.value = C.jsonToTecNodeAST(
    JSON.parse(
      await useAppStore().haskell!.exports.encodeHaskellNode(haskellCode.value),
    ),
  );
}

async function updateCode() {
  if (tecNodeAST.value)
    haskellCode.value = await useAppStore().haskell!.exports.decodeHaskellNode(
      JSON.stringify(C.tecNodeASTToJson(tecNodeAST.value)),
    );
}
</script>

<template>
  <v-container fluid>
    <v-row>
      <v-col cols="12">
        <div class="d-flex flex-row ga-2 mb-4">
          <v-btn @click="fillSampleCode">fill preset schema - clo3d</v-btn>
        </div>
      </v-col>

      <v-col cols="12">
        <v-textarea
          v-model="haskellCode"
          auto-grow
          class="font-mono"
          label="TecLang"
          variant="outlined"
        />
      </v-col>

      <v-col cols="12">
        <div class="d-flex ga-2">
          <v-btn color="primary" @click="updateUI">update ui</v-btn>
          <v-btn color="primary" @click="updateCode">update code</v-btn>
        </div>
      </v-col>

      <TecNodeAST
        v-model="tecNodeAST"
        :tec-enums="props.tecEnumAst"
        :tec-classes="props.tecClassAst"
      />
      <v-col cols="12" v-if="tecNodeAST">
        <v-btn color="primary" @click="$emit('update', tecNodeAST)">GO!</v-btn>/
      </v-col>
    </v-row>
  </v-container>
</template>

<style scoped lang="sass"></style>
