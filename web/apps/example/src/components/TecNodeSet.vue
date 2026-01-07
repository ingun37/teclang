<script setup lang="ts">
import * as C from "codec";
import InputRecurse from "@/components/InputRecurse.vue";
import * as E from "effect";

const model = defineModel<C.TecNode.TecNodeSet>({ required: true });

const props = defineProps<{
  tecEnums: C.TecEnum.TecEnumAST;
  tecClasses: C.TecClass.TecClassAST;
}>();

const tecClass = computed(() => {
  const found = E.Array.findFirst(
    props.tecClasses.tecClasses,
    (c) => c.tecClassName === model.value.tecNodeClass,
  );
  return E.Effect.runSync(
    E.Either.fromOption(
      found,
      () => new Error("Class not found: " + model.value.tecNodeClass),
    ),
  );
});
const dimension = computed(() => {
  const enums = E.pipe(
    tecClass.value.tecSignature.indexTypeSet,
    E.Array.filterMap((pt) => {
      return E.pipe(
        props.tecEnums.tecEnums,
        E.Array.findFirst(
          (te) => (te.tecEnumName as string) === (pt as string),
        ),
      );
    }),
  );
  const names = enums.map((x) => x.tecEnumName + `(${x.tecEnumValues.length})`);
  return `${names.join(" x ")}`;
});
function inputUpdate(
  indexCombo: C.TecEnum.TecEnumValue[],
  paramTypeIndex: number,
  value: any,
) {
  const foundIdx = E.Array.findFirstIndex(model.value.tecNodeSet, (x) => {
    return E.Array.getEquivalence(C.TecEnum.TecEnumValueEquivalence)(
      x.indexCombination,
      indexCombo,
    );
  });

  E.Effect.runSync(
    E.Either.gen(function* () {
      const idx = yield* E.Either.fromOption(
        foundIdx,
        () => new Error("Failed to find node at " + indexCombo.join(", ")),
      );
      const attribs = model.value.tecNodeSet[idx]!.tecNodeAttributes;
      model.value = C.TecNode.TecNodeSet.make({
        tecNodeClass: model.value.tecNodeClass,
        tecNodeSet: E.Array.replace(
          model.value.tecNodeSet,
          idx,
          C.TecNode.TecNode.make({
            indexCombination: indexCombo,
            tecNodeAttributes: E.Array.replace(attribs, paramTypeIndex, value),
          }),
        ),
      });
    }),
  );
}
</script>

<template>
  <v-card>
    <v-card-subtitle
      >Input {{ model.tecNodeClass }} class data for each
      {{ dimension }}</v-card-subtitle
    >
    <v-card-text>
      <InputRecurse
        :index-combo="[]"
        axis="x"
        :param-index="0"
        :tec-indexed-class="tecClass"
        :tec-enums="tecEnums"
        @update="(x, y, z) => inputUpdate(x, y, z)"
      />
    </v-card-text>
  </v-card>
</template>

<style scoped lang="sass"></style>
