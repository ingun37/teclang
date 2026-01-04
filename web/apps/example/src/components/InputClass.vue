<script setup lang="ts">
import * as C from "codec";
import * as E from "effect";
import InputRecurse from "@/components/InputRecurse.vue";
const props = defineProps<{
  tecClass: C.TecClass.TecClass;
  tecEnums: C.TecEnum.TecEnumAST;
}>();

const dimension = computed(() => {
  const enums = E.pipe(
    props.tecClass.tecSignature.indexSet,
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
</script>

<template>
  <v-card>
    <v-card-subtitle
      >Input {{ tecClass.tecClassName }} class data for each
      {{ dimension }}</v-card-subtitle
    >
    <v-card-text>
      <InputRecurse
        :index-combo="[]"
        axis="x"
        :param-index="0"
        :tec-indexed-class="tecClass"
        :tec-enums="tecEnums"
      />
    </v-card-text>
  </v-card>
</template>

<style scoped lang="sass"></style>
