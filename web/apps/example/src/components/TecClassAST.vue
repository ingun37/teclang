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
          attributeTypeSet: [C.TecClass.TecClassAttribute.make("String")],
          indexTypeSet: [],
        }),
      }),
    ),
  });
}
</script>

<template>
  <v-container fluid>
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
</template>

<style scoped lang="sass"></style>
