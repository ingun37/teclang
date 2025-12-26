<script setup lang="ts">
const model = defineModel<string>({ required: true });
const intermediateName = ref(model.value);

function updateParamType(newName: string) {
  model.value = newName;
}
</script>

<template>
  <v-text-field
    v-model="intermediateName"
    compact
    hide-details
    density="compact"
    label="Parameter Type"
    class="mb-2"
    @keyup.enter="updateParamType(intermediateName)"
  >
    <template #append-inner>
      <v-btn
        icon="mdi-check"
        variant="text"
        density="compact"
        @click="updateParamType(intermediateName)"
      />
    </template>
  </v-text-field>
</template>

<style scoped lang="sass"></style>
