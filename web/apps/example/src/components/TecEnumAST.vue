<script setup lang="ts">
import * as C from "codec";
import * as E from "effect";
import TecEnum from "@/components/TecEnum.vue";
const model = defineModel<C.TecEnum.TecEnumAST>({ required: true });
function addEnum() {
  model.value = C.TecEnum.TecEnumAST.make({
    tecEnums: E.Array.append(
      model.value.tecEnums,
      C.TecEnum.TecEnum.make({
        tecEnumName: C.TecEnum.TecEnumName.make("Foo"),
        tecEnumValues: ["A", "B", "C"].map((x) =>
          C.TecEnum.TecEnumValue.make(x),
        ),
        tecEnumRepresentation: C.TecEnum.TecEnumRepresentation.make({
          repAttribs: [],
        }),
      }),
    ),
  });
}
</script>

<template>
  <v-card>
    <v-card-subtitle>Enums</v-card-subtitle>
    <v-card-text>
      <v-container>
        <v-row dense>
          <v-col cols="auto" v-for="(_, i) in model.tecEnums" :key="i">
            <TecEnum v-model="model.tecEnums[i]!" />
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
