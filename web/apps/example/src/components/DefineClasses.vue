<script setup lang="ts">
const model = defineModel<string[]>({ required: true });

function addClass() {
  model.value = model.value.concat(["NewClass"]);
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
