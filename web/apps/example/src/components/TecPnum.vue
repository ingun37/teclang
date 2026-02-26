<script setup lang="ts">
import * as C from "codec";
import * as E from "effect";
import TecPnumTable from "@/components/TecPnumTable.vue";

const model = defineModel<C.TecPnum.TecPnum>({ required: true });
function lensOver(f: (x: C.TecPnum.TecPnum) => C.TecPnum.TecPnum) {
  model.value = f(model.value);
}
const addEnumValue = () => {
  lensOver(C.TecPnum.lens.tecPnum.over._tecEnumValues(E.Array.append("Foo")));
};
const deleteEnumValue = (index: number) => {
  lensOver(
    C.TecPnum.lens.tecPnum.over._tecEnumValues((xs) => {
      const xs2 = E.Array.remove(xs, index);
      if (!E.Array.isNonEmptyReadonlyArray(xs2)) return xs;
      else return xs2;
    }),
  );
};
</script>

<template>
  <v-card>
    <v-card-title>{{ model._tecPnumName }}</v-card-title>
    <v-card-text>
      <div class="d-flex flex-row ga-2">
        <div class="d-flex flex-column ga-2">
          <v-text-field
            v-for="(_, i) in model._tecEnumValues"
            :key="i"
            v-model="model._tecEnumValues[i]"
            append-inner-icon="mdi-delete"
            :disabled="model._tecEnumValues.length <= 1"
            @click:append-inner="deleteEnumValue(i)"
          ></v-text-field>
          <v-btn @click="addEnumValue">Add Enum Value</v-btn>
        </div>
        <TecPnumTable
          v-if="model._tecPnumTable"
          v-model="model._tecPnumTable"
        />
      </div>
    </v-card-text>
  </v-card>
</template>

<style scoped lang="sass"></style>
