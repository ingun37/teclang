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
function makeDynamicModel(oneComb: C.help.IndexEnumValueCombo) {
  const indexEnumValueSets: RNE<C.help.IndexEnumValueCombo[]> = C.help
    .iterateNodesInNodeSet(props.enumAst, props.tecClass)
    .force(model.value);

  const contains = E.Array.containsWith(C.help.indexEnumValueComboEq);

  for (let i = 0; i < indexEnumValueSets.length; i++) {
    const tn = model.value[i]!;
    if (contains(indexEnumValueSets[i]!, oneComb)) {
      return computed<TN | null>({
        get: () => tn,
        set(newTN: TN | null) {
          if (newTN) model.value = E.Array.replace(model.value, i, newTN);
          else {
            const ys = E.Array.remove(model.value, i);
            if (E.Array.isNonEmptyArray(ys)) model.value = ys;
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
const indexEnumValueCombos = computed<
  E.Either.Either<
    { rows: RNE<C.TecEnum.TecEnumValue>; cols: RNE<C.TecEnum.TecEnumValue> },
    Error
  >
>(() => {
  const find = C.help.makeFindEnum(props.enumAst).either;
  return E.Either.gen(function* () {
    const rows: C.TecEnum.TecEnum = yield* find(
      props.tecClass.tecSignature.indexTypeSet[0]!,
    );

    const cols: C.TecEnum.TecEnum = yield* find(
      props.tecClass.tecSignature.indexTypeSet[1]!,
    );

    return {
      rows: rows.tecEnumValues,
      cols: cols.tecEnumValues,
    };
  });
});
function toNodeIndex(ev: C.TecEnum.TecEnumValue) {
  return C.TecNode.TecNodeIndexInst.make({ contents: ev });
}
</script>

<template>
  <div
    v-if="E.Either.isRight(indexEnumValueCombos)"
    class="d-flex flex-row ga-1"
  >
    <v-table>
      <thead>
        <tr>
          <th></th>
          <th v-for="col in indexEnumValueCombos.right.cols">
            {{ col }}
          </th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="row in indexEnumValueCombos.right.rows" :key="row">
          <td>
            {{ row }}
          </td>
          <td v-for="col in indexEnumValueCombos.right.cols" :key="col">
            <TecNodeNullable
              :tec-class="tecClass"
              :enum-ast="enumAst"
              :comb="[toNodeIndex(row), toNodeIndex(col)]"
              v-model="makeDynamicModel([row, col]).value"
            />
          </td>
        </tr>
      </tbody>
    </v-table>
  </div>
  <v-alert
    v-else
    :text="`${indexEnumValueCombos.left}`"
    title="Error"
    type="error"
  />
</template>

<style scoped lang="sass"></style>
