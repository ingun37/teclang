<script setup lang="ts">
import * as C from "codec";
import { useAppStore } from "@/stores/app.ts";

const props = defineProps<{
  tecClassAst: C.TecClass.TecClassAST;
  tecEnumAst: C.TecEnum.TecEnumAST;
}>();
const sqlCode = ref("");
function generateSql() {
  sqlCode.value = C.help.generateSqlSchema(props.tecEnumAst, props.tecClassAst);
}
async function runSql() {
  if (sqlCode) {
    useAppStore().db!.run(sqlCode.value);
  }
}
</script>

<template>
  <v-container fluid>
    <v-row>
      <v-col cols="12">
        <div class="d-flex flex-row ga-2 mb-4">
          <v-btn @click="generateSql">Generate Sql</v-btn>
          <v-btn v-if="sqlCode" @click="runSql">Run Sql</v-btn>
        </div>
      </v-col>
      <v-col cols="12">
        <v-textarea
          v-model="sqlCode"
          auto-grow
          class="font-mono"
          label="TecLang"
          variant="outlined"
        />
      </v-col>
    </v-row>
  </v-container>
</template>

<style scoped lang="sass"></style>
