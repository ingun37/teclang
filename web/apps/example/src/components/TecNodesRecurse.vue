<script setup lang="ts">
import * as C from "codec";
import type { Axis } from "@/Axis.ts";
import * as E from "effect";
import TecNode from "@/components/TecNode.vue";

type RNE<T> = E.Array.NonEmptyReadonlyArray<T>;
type TN = C.TecNode.TecNode;
const model = defineModel<RNE<TN>>({ required: true });

const props = defineProps<{
  axis: Axis;
  paramIndex: number;
  tecIndexedClass: C.TecClass.TecClass;
  tecEnums: C.TecEnum.TecEnumAST;
}>();
const dynamicSlices = computed(() => {
  const eq = C.TecEnum.TecEnumValueEquivalence;
  const i = props.paramIndex;
  const groups: RNE<RNE<TN>> = E.pipe(
    model.value,
    E.Array.groupWith((x, y) =>
      eq(x.indexCombination[i]!, y.indexCombination[i]!),
    ),
  );
  const groupLens = E.Array.scan(
    groups.map((x) => x.length),
    0,
    (x, y) => x + y,
  );

  return E.Array.map(groups, (group, gi) => {
    return computed<RNE<TN>>({
      get: (): RNE<TN> => group,
      set(chunk: RNE<TN>) {
        const left = E.Array.take(model.value, groupLens[gi]!);
        const right = E.Array.drop(model.value, groupLens[gi + 1]!);
        model.value = E.Array.appendAll(E.Array.appendAll(left, chunk), right);
      },
    });
  });
});
</script>

<template>
  <div
    v-if="1 < model.length"
    :class="['d-flex', axis === 'x' ? 'flex-row' : 'flex-column']"
  >
    <TecNodesRecurse
      v-for="(slice, idx) in dynamicSlices"
      :key="idx"
      v-model="slice.value"
      :axis="axis === 'x' ? 'y' : 'x'"
      :paramIndex="paramIndex + 1"
      :tecIndexedClass="tecIndexedClass"
      :tecEnums="tecEnums"
    />
  </div>
  <div v-else class="d-flex flex-column ga-1">
    <TecNode
      v-model="model[0]"
      :enable-index-list-editing="false"
      :tec-indexed-class="tecIndexedClass"
      :all-enums="tecEnums"
    />
  </div>
</template>

<style scoped lang="sass"></style>
