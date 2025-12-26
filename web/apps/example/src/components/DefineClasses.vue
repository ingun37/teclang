<script setup lang="ts">
import * as C from "codec";
const model = defineModel<C.TecType.TecClass[]>({ required: true });

function addClass() {
  model.value = model.value.concat([
    C.TecType.TecClass.make({ className: "NewClass", parameterTypes: [] }),
  ]);
}
</script>

<template>
  <div class="ml-4 mb-4">
    <div class="text-caption mb-2">Classes</div>
    <DefineClass v-for="(_, i) in model" v-model="model[i]!" :key="i" />

    <v-btn
      prepend-icon="mdi-plus"
      variant="plain"
      size="small"
      @click="addClass"
    >
      Add Class
    </v-btn>
  </div>
</template>

<style scoped lang="sass"></style>
