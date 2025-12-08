<script lang="ts" setup>
import type { TecAST as TecASTType } from "@/schema/TecAST";
import TecAST from "@/components/TecAST.vue";

interface Props {
  items: readonly TecASTType[];
}

const props = defineProps<Props>();
</script>

<template>
  <v-container class="pa-0">
    <v-row no-gutters>
      <v-col cols="12">
        <div class="d-flex flex-column">
          <TecAST v-for="(item, index) in items" :key="index" :ast="item" />
        </div>
      </v-col>
    </v-row>
  </v-container>
</template>

<style lang="sass" scoped></style>
