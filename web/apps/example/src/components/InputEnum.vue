<script setup lang="ts">
import * as C from "codec";
defineProps<{
  tecEnum: C.TecEnum.TecEnum;
  allEnums: C.TecEnum.TecEnumAST;
}>();
</script>

<template>
  <v-card>
    <v-card-subtitle
      >Input attributes of {{ tecEnum.tecEnumName }} enum</v-card-subtitle
    >
    <v-card-text>
      <div class="d-flex flex-column">
        <div v-for="tecEnumValue in tecEnum.tecEnumValues">
          <span class="text-caption">{{ `${tecEnumValue}: ` }}</span>
          <InputEnumRepAttrib
            v-for="x in tecEnum.tecEnumRepresentation.repAttribs"
            :rep-attrib="x"
            :allEnums="allEnums"
          />
        </div>
      </div>
    </v-card-text>
  </v-card>
</template>

<style scoped lang="sass"></style>
