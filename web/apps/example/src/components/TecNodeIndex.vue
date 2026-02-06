<script setup lang="ts">
import * as C from "codec";
import * as E from "effect";
const props = defineProps<{
  tecIndexedClass: C.TecClass.TecClass;
  tecClassIndex: C.TecClass.TecClassIndex;
  allEnums: C.TecEnum.TecEnumAST;
  enableIndexListEditing: boolean;
}>();

const model = defineModel<C.TecNode.TecNodeIndex>({ required: true });

type IndexOption = {
  label: string;
  value: C.TecNode.TecNodeIndex;
};
const options = computed((): E.Either.Either<IndexOption[], Error> => {
  return E.Either.gen(function* () {
    const tecEnumO = E.Array.findFirst(
      props.allEnums.tecEnums,
      (enums) =>
        (enums.tecEnumName as string) === (props.tecClassIndex as string),
    );

    const tecEnum = yield* E.Either.fromOption(
      tecEnumO,
      () => new Error("Enum not found: " + props.tecClassIndex),
    );

    const insts = E.Array.map(tecEnum.tecEnumValues, (val): IndexOption => {
      const inst = C.TecNode.TecNodeIndexInst.make({ contents: val });
      return {
        label: val,
        value: inst,
      };
    });

    const wildcard: IndexOption = {
      label: "_ (match all)",
      value: C.TecNode.TecNodeIndexWildcard.make({}),
    };

    return E.Array.prepend(insts, wildcard);
  });
});
</script>

<template>
  <v-alert
    v-if="E.Either.isLeft(options)"
    :text="`${options.left}`"
    title="Error"
    type="error"
  ></v-alert>
  <v-select
    v-else
    v-model="model"
    :items="options.right"
    item-title="label"
    item-value="value"
    density="compact"
    hide-details
  />
</template>

<style scoped lang="sass"></style>
