<script setup lang="ts">
import * as C from "codec";
const model = defineModel<readonly C.TecType.TecClass[]>({ required: true });

function addClass() {
  model.value = model.value.concat([
    C.TecType.TecClass.make({ className: "NewClass", parameterTypes: [] }),
  ]);
}
</script>

<template>
  <v-card>
    <v-card-subtitle>Classes</v-card-subtitle>
    <v-card-text>
      <v-container>
        <v-row dense>
          <v-col cols="auto" v-for="(_, i) in model" :key="i">
            <DefineClass v-model="model[i]!" />
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
