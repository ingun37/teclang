<script setup lang="ts">
import * as E from "effect";
import * as C from "codec";
const model = defineModel<C.TecEnum.TecEnum>({ required: true });
defineProps<{
  possibleOptions: string[];
}>();
function addValue() {
  model.value = C.TecEnum.TecEnum.make({
    tecEnumName: model.value.tecEnumName,
    tecEnumValues: E.Array.append(
      model.value.tecEnumValues,
      C.TecEnum.TecEnumValue.make("Foo"),
    ),
  });
}
</script>

<template>
  <v-card>
    <v-card-text>
      <div class="d-flex flex-column ga-2">
        <v-text-field
          v-model="model.tecEnumName"
          label="Enum Name"
          compact
          hide-details
          density="compact"
          class="mb-2"
          variant="underlined"
        />

        <div class="text-caption mb-2">Values</div>
        <v-text-field
          v-for="(_, index) in model.tecEnumValues"
          v-model="model.tecEnumValues[index]"
          density="compact"
          hide-details
          style="width: 6rem"
          variant="outlined"
        />
        <v-btn
          prepend-icon="mdi-plus"
          variant="plain"
          size="small"
          @click="addValue"
        >
          Add Value
        </v-btn>

        <v-divider class="my-2" />

        <div class="text-caption mb-2">Representation Attributes</div>
      </div>
    </v-card-text>
  </v-card>
</template>

<style scoped lang="sass"></style>
