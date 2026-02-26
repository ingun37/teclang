<script setup lang="ts">
import * as C from "codec";
import * as E from "effect";

const model = defineModel<C.TecPnum.TecPnumTable>({ required: true });

function lensOver(f: (x: C.TecPnum.TecPnumTable) => C.TecPnum.TecPnumTable) {
  model.value = f(model.value);
}

const addIndexType = () => {
  lensOver(
    C.TecPnum.lens.tecPnumTable.over._tecIndexTypes(E.Array.append("NewType")),
  );
};

const deleteIndexType = (index: number) => {
  lensOver(
    C.TecPnum.lens.tecPnumTable.over._tecIndexTypes((xs) => {
      const xs2 = E.Array.remove(xs, index);
      if (!E.Array.isNonEmptyReadonlyArray(xs2)) return xs;
      else return xs2;
    }),
  );
};
</script>

<template>
  <div class="d-flex flex-column ga-2">
    <div class="d-flex flex-row ga-2">
      <v-text-field
        v-for="(_, i) in model._tecIndexTypes"
        :key="i"
        v-model="model._tecIndexTypes[i]"
        append-inner-icon="mdi-delete"
        :disabled="model._tecIndexTypes.length <= 1"
        @click:append-inner="deleteIndexType(i)"
      ></v-text-field>
      <v-btn @click="addIndexType">Add index type</v-btn>
    </div>
  </div>
</template>

<style scoped lang="sass"></style>
