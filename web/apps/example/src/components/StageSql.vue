<script setup lang="ts">
import * as C from "codec";
import { useAppStore } from "@/stores/app.ts";

const props = defineProps<{
  tecClassAst: C.TecClass.TecClassAST;
  tecEnumAst: C.TecEnum.TecEnumAST;
  tecNodeAst: C.TecNode.TecNodeAST;
}>();
const sqlCode = ref("");
function generateSql() {
  sqlCode.value = C.help.generateSqlSchema(
    props.tecEnumAst,
    props.tecClassAst,
    props.tecNodeAst,
  );
}
const sqlIsSuccessful = ref(false);
async function runSql() {
  if (sqlCode) {
    try {
      useAppStore().db!.run(sqlCode.value);
      sqlIsSuccessful.value = true;
    } catch (e) {
      console.error(e);
      sqlIsSuccessful.value = false;
    }
  }
}

function downloadDbFile(filename = "database.sqlite") {
  const db = useAppStore().db;
  if (!db) return;

  // sql.js returns the SQLite file bytes
  const bytes: Uint8Array = db.export(); // Uint8Array
  const copy = new Uint8Array(bytes);

  const blob = new Blob([copy], { type: "application/x-sqlite3" });

  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = filename;
  a.click();

  // cleanup
  setTimeout(() => URL.revokeObjectURL(url), 0);
}
</script>

<template>
  <v-container fluid>
    <v-row>
      <v-col cols="12">
        <div class="d-flex flex-row ga-2 mb-4">
          <v-btn @click="generateSql">Generate Sql</v-btn>
          <v-btn v-if="sqlCode" @click="runSql">Run Sql</v-btn>
          <v-btn v-if="sqlIsSuccessful" @click="downloadDbFile()"
            >Download DB</v-btn
          >
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
