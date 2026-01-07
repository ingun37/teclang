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

const haskellCode = ref("");
function createInitialHaskellCode() {
  const combinations = C.help.iterateIndexSet(props.tecEnumAst);
  const a = E.Effect.runSync(
    E.pipe(
      props.tecClassAst.tecClasses,
      E.Array.map((c) =>
        E.Either.gen(function* () {
          const combs = yield* combinations(c);
          const each = combs.map(
            (comb) =>
              comb.map((v) => `${v}: `).join("") +
              c.tecSignature.indexTypeSet.map(() => "A").join(" "),
          );
          return `${c.tecClassName} = [\n${each.map((x) => "  " + x).join(",\n")}]`;
        }),
      ),
      E.Either.all,
    ),
  );
  return a.join("\n");
}
const sampleHaskellCode = createInitialHaskellCode();

function fillSampleCode() {
  haskellCode.value = sampleHaskellCode;
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
  <v-container>
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
    </v-row>
  </v-container>
</template>

<style scoped lang="sass"></style>
