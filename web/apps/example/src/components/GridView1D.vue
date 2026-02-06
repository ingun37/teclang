<script setup lang="ts">
import * as C from "codec";
import * as E from "effect";
import TecNodeNullable from "@/components/TecNodeNullable.vue";
type RNE<T> = E.Array.NonEmptyReadonlyArray<T>;
type TN = C.TecNode.TecNode;
const model = defineModel<RNE<TN>>({ required: true });

const props = defineProps<{
  tecClass: C.TecClass.TecClass;
  enumAst: C.TecEnum.TecEnumAST;
}>();
function makeDynamicModel(ev: C.TecEnum.TecEnumValue) {
  const iterCombo = C.help.iterateIndexCombo(
    props.enumAst,
    props.tecClass.tecSignature.indexTypeSet,
  );
  const contains = E.Array.containsWith(C.help.indexEnumValueComboEq);

  for (let i = 0; i < model.value.length; i++) {
    const tn = model.value[i]!;
    if (contains(E.Effect.runSync(iterCombo(tn.indexCombination)), [ev])) {
      return computed<TN | null>({
        get: () => tn,
        set(newTN: TN | null) {
          if (newTN) model.value = E.Array.replace(model.value, i, newTN);
          else {
            const newArr = E.Array.remove(model.value, i);
            if (E.Array.isNonEmptyArray(newArr)) model.value = newArr;
            else throw new Error("Cannot remove empty grid");
          }
        },
      });
    }
  }
  return computed<TN | null>({
    get: () => null,
    set(newTN) {
      if (newTN) model.value = E.Array.append(model.value, newTN);
    },
  });
}

const indexEnumValues = computed(() => {
  return E.Either.gen(function* () {
    const enumDef: C.TecEnum.TecEnum = yield* E.Either.fromOption(
      C.help.findEnumOfIndexType(props.enumAst)(
        props.tecClass.tecSignature.indexTypeSet[0]!,
      ),
      () =>
        new Error(
          "Failed to find enum:" + props.tecClass.tecSignature.indexTypeSet[0],
        ),
    );

    return E.Array.map(enumDef.tecEnumValues, (ev) =>
      C.TecNode.TecNodeIndexInst.make({ contents: ev }),
    );
  });
});
</script>

<template>
  <div v-if="E.Either.isRight(indexEnumValues)" class="d-flex flex-row ga-1">
    <TecNodeNullable
      v-for="ev in indexEnumValues.right"
      :tec-class="tecClass"
      :enum-ast="enumAst"
      :comb="[ev]"
      v-model="makeDynamicModel(ev.contents).value"
    />
  </div>
</template>

<style scoped lang="sass"></style>
