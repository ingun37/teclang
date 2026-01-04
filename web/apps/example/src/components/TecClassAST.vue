<script setup lang="ts">
import * as C from "codec";
import * as E from "effect";
import TecClass from "@/components/TecClass.vue";
const model = defineModel<C.TecClass.TecClassAST>({
  required: true,
});
const props = defineProps<{ tecEnumAst: C.TecEnum.TecEnumAST }>();

function addClass() {
  model.value = C.TecClass.TecClassAST.make({
    tecClasses: E.Array.append(
      model.value.tecClasses,
      C.TecClass.TecClass.make({
        tecClassName: C.TecClass.TecClassName.make("newClass"),
        tecSignature: C.TecClass.TecSignature.make({
          attributes: [C.TecClass.TecClassAttribute.make("String")],
          indexSet: [],
        }),
      }),
    ),
  });
}
</script>

<template>
  <v-card>
    <v-card-subtitle>Classes</v-card-subtitle>
    <v-card-text>
      <v-container>
        <v-row dense>
          <v-col cols="auto" v-for="(_, i) in model.tecClasses" :key="i">
            <TecClass
              v-model="model.tecClasses[i]!"
              :tec-enum-ast="props.tecEnumAst"
            />
          </v-col>
          <v-col cols="auto">
            <v-card>
              <v-card-text>
                <v-btn
                  prepend-icon="mdi-plus"
                  variant="plain"
                  size="small"
                  @click="addClass"
                >
                  Add Class
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
