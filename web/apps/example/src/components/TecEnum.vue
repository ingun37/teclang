<script setup lang="ts">
import * as E from "effect";
import * as C from "codec";
const model = defineModel<C.TecEnum.TecEnum>({ required: true });

function addValue() {
  model.value = C.TecEnum.TecEnum.make({
    tecEnumName: model.value.tecEnumName,
    tecEnumValues: E.Array.append(
      model.value.tecEnumValues,
      C.TecEnum.TecEnumValue.make("Foo"),
    ),
    tecEnumRepresentation: model.value.tecEnumRepresentation,
  });
}

function addAttribute() {
  model.value = C.TecEnum.TecEnum.make({
    tecEnumName: model.value.tecEnumName,
    tecEnumValues: model.value.tecEnumValues,
    tecEnumRepresentation: C.TecEnum.TecEnumRepresentation.make({
      repAttribs: E.Array.append(
        model.value.tecEnumRepresentation.repAttribs,
        C.TecEnum.TecEnumRepresentationAttribute.make({
          repAttribKey:
            C.TecEnum.TecEnumRepresentationAttributeKey.make("newKey"),
          repAttribType:
            C.TecEnum.TecEnumRepresentationAttributeType.make("String"),
        }),
      ),
    }),
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
        <div
          v-for="(_, index) in model.tecEnumRepresentation.repAttribs"
          :key="index"
          class="d-flex ga-2"
        >
          <v-text-field
            v-model="
              model.tecEnumRepresentation.repAttribs[index]!.repAttribKey
            "
            label="Key"
            density="compact"
            hide-details
            variant="outlined"
          />
          <v-text-field
            v-model="
              model.tecEnumRepresentation.repAttribs[index]!.repAttribType
            "
            label="Type"
            density="compact"
            hide-details
            variant="outlined"
          />
        </div>
        <v-btn
          prepend-icon="mdi-plus"
          variant="plain"
          size="small"
          @click="addAttribute"
        >
          Add Attribute
        </v-btn>
      </div>
    </v-card-text>
  </v-card>
</template>

<style scoped lang="sass"></style>
