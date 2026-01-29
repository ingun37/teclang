<script setup lang="ts">
import * as C from "codec";
import TecSignature from "@/components/TecSignature.vue";
const model = defineModel<C.TecClass.TecClass>({ required: true });
defineProps<{ tecEnumAst: C.TecEnum.TecEnumAST }>();
const emit = defineEmits<{
  (e: "remove"): void;
}>();
</script>

<template>
  <v-card>
    <v-card-text>
      <div class="d-flex flex-column ga-1">
        <v-text-field
          v-model="model.tecClassName"
          label="Class Name"
          compact
          hide-details
          density="compact"
          variant="underlined"
        />
        <TecSignature :tec-enum-ast="tecEnumAst" v-model="model.tecSignature" />
        <v-btn
          prepend-icon="mdi-delete"
          variant="plain"
          size="small"
          color="error"
          @click="emit('remove')"
        >
          Remove
        </v-btn>
      </div>
    </v-card-text>
  </v-card>
</template>

<style scoped lang="sass"></style>
