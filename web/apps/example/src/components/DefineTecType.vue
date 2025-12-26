<script setup lang="ts">
import * as C from "codec";
import DefineClasses from "@/components/DefineClasses.vue";

const model = defineModel<C.TecType.TecType>({ required: true });
const intermediateName = ref(model.value.tecTypeName);
function updateTecTypeName(newName: string) {
  model.value = C.TecType.TecType.make({
    tecTypeName: newName,
    classes: model.value.classes,
  });
}
const classNames = computed({
  get: () => model.value.classes.map((c) => c.className),
  set: (names) => {
    model.value = C.TecType.TecType.make({
      tecTypeName: model.value.tecTypeName,
      classes: names.map((name) => ({ className: name, parameterTypes: [] })),
    });
  },
});
</script>

<template>
  <v-text-field
    v-model="intermediateName"
    compact
    hide-details
    density="compact"
    label="Tec Type Name"
    @keyup.enter="updateTecTypeName(intermediateName)"
  >
    <template #append-inner>
      <v-btn
        icon="mdi-check"
        variant="text"
        density="compact"
        @click="updateTecTypeName(intermediateName)"
      />
    </template>
  </v-text-field>
  <DefineClasses v-model="classNames" />
</template>

<style scoped lang="sass"></style>
