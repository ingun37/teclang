<script setup lang="ts">
import * as C from "codec";

const model = defineModel<readonly C.TecType.TecEnum[]>({ required: true });
function addEnum() {
  model.value = [
    ...model.value,
    C.TecType.TecEnum.make({
      tecTypeName: "Foo",
      values: ["A", "B", "C"].map((x) => C.TecType.TecEnumValue.make(x)),
    }),
  ];
}
</script>

<template>
  <v-card>
    <v-card-subtitle>Enums</v-card-subtitle>
    <v-card-text>
      <v-container>
        <v-row dense>
          <v-col cols="auto" v-for="(_, i) in model" :key="i">
            <DefineTecEnum v-model="model[i]!" />
          </v-col>
          <v-col cols="auto">
            <v-card>
              <v-card-text>
                <v-btn
                  prepend-icon="mdi-plus"
                  variant="plain"
                  size="small"
                  @click="addEnum"
                >
                  Add Enum
                </v-btn>
              </v-card-text>
            </v-card>
          </v-col>
        </v-row>
      </v-container>
    </v-card-text>
  </v-card>
</template>

<style scoped lang="sass"></style>
