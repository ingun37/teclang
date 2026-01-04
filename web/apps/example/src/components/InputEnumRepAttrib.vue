<script setup lang="ts">
import * as E from "effect";
import * as C from "codec";

const props = defineProps<{
  repAttrib: C.TecEnum.TecEnumRepresentationAttribute;
  allEnums: C.TecEnum.TecEnumAST;
}>();
const options = computed<readonly string[]>(() => {
  return E.Effect.runSync(
    E.pipe(
      props.allEnums.tecEnums,
      E.Array.findFirst(
        (tecEnum) =>
          (tecEnum.tecEnumName as string) === props.repAttrib.repAttribType,
      ),
      E.Either.fromOption(() => new Error("Failed to find enum with the type")),
      E.Either.map((foundEnum) => foundEnum.tecEnumValues),
    ),
  );
});
</script>

<template>
  <div class="d-flex flex-row ga-2">
    <span class="text-caption">{{ repAttrib.repAttribKey }}</span>
    <v-select
      :items="options"
      density="compact"
      hide-details
      variant="outlined"
      style="width: 6rem"
    />
  </div>
</template>

<style scoped lang="sass"></style>
