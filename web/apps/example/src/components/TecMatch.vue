<script setup lang="ts">
import * as C from "codec";
import * as E from "effect";
const model = defineModel<C.TecPnum.TecMatch>({ required: true });
type RNE<T> = E.Array.NonEmptyReadonlyArray<T>;
const props = defineProps<{
  pnumAst: C.TecPnum.TecPnumAST;
  pnum: C.TecPnum.TecPnum;
  pnumTable: C.TecPnum.TecPnumTable;
  enumValuesForEachIndexType: RNE<RNE<string>>;
}>();
const optionsForEachIndexType = computed(
  (): RNE<RNE<C.TecPnum.TecIndexPattern>> => {
    return E.pipe(
      props.enumValuesForEachIndexType,
      E.Array.map((enumValues) => {
        return E.pipe(
          enumValues,
          E.Array.map((enumValue) =>
            C.TecPnum.TecIndexValue.make({ contents: enumValue }),
          ),
          E.Array.prepend(C.TecPnum.TecIndexAll.make()),
        );
      }),
    );
  },
);
function itemProps(pattern: C.TecPnum.TecIndexPattern) {
  return pattern.tag === "TecIndexAll"
    ? {
        title: "_ (match all)",
      }
    : {
        title: pattern.contents,
      };
}
</script>

<template>
  <div class="d-flex flex-row ga-2">
    <v-select
      v-for="(indexType, i) in pnumTable._tecIndexTypes"
      :key="i"
      v-model="model._tecIndexPatterns[i]"
      :items="optionsForEachIndexType[i]"
      :item-props="itemProps"
      append-inner-icon="mdi-delete"
    ></v-select>
  </div>
</template>

<style scoped lang="sass"></style>
