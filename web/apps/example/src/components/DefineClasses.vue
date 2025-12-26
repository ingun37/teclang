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
  <div class="ml-4 mb-4">
    <div class="text-caption mb-2">Classes</div>
    <v-row dense>
      <v-col v-for="(_, i) in model" :key="i" cols="auto">
        <DefineClass v-model="model[i]!" />
      </v-col>

      <v-col cols="auto" class="d-flex align-center">
        <v-btn
          prepend-icon="mdi-plus"
          variant="plain"
          size="small"
          @click="addClass"
        >
          Add Class
        </v-btn>
      </v-col>
    </v-row>
  </div>
</template>

<style scoped lang="sass"></style>
