<script setup lang="ts">
import * as C from "codec";
import DefineIndexSet from "@/components/DefineIndexSet.vue";
const model = defineModel<C.TecType.TecIndexedClass>({ required: true });
const props = defineProps<{ tecEnums: readonly C.TecType.TecEnum[] }>();
</script>

<template>
  <v-card>
    <v-card-text>
      <div class="d-flex flex-column ga-1">
        <v-text-field
          v-model="model.className"
          label="Class Name"
          compact
          hide-details
          density="compact"
          variant="underlined"
        />
        <v-select
          v-model="model.paramType"
          :items="['String', 'Number', 'Image']"
          density="compact"
          hide-details
          variant="outlined"
        />

        <DefineIndexSet v-model="model.indexSet" :tecEnums="props.tecEnums" />
      </div>
    </v-card-text>
  </v-card>
</template>

<style scoped lang="sass"></style>
