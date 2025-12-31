<script setup lang="ts">
import * as C from "codec";
import * as E from "effect";
import InputRecurse from "@/components/InputRecurse.vue";
const props = defineProps<{
  tecIndexedClass: C.TecType.TecIndexedClass;
  tecEnums: readonly C.TecType.TecEnum[];
}>();

const dimension = computed(() => {
  const enums = E.pipe(
    props.tecIndexedClass.indexSet,
    E.Array.filterMap((pt) => {
      return E.pipe(
        props.tecEnums,
        E.Array.findFirst((te) => te.tecTypeName === pt),
      );
    }),
  );
  const names = enums.map((x) => x.tecTypeName + `(${x.values.length})`);
  return `${names.join(" x ")}`;
});
</script>

<template>
  <v-card>
    <v-card-subtitle
      >Input {{ tecIndexedClass.className }} class data for each
      {{ dimension }}</v-card-subtitle
    >
    <v-card-text>
      <InputRecurse
        axis="x"
        :param-index="0"
        :tec-indexed-class="tecIndexedClass"
        :tec-enums="tecEnums"
      />
    </v-card-text>
  </v-card>
</template>

<style scoped lang="sass"></style>
