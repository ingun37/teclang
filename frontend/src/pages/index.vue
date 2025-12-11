<template>
  <v-container>
    <v-row
      ><v-btn v-on:click="formatCode">Format code</v-btn>
      <v-btn v-if="tecAST === null" v-on:click="createHStack"
        >create hstack</v-btn
      >
    </v-row>
    <v-row>
      <v-col cols="12">
        <v-textarea
          v-model="appStore.textValue"
          clearable
          label="Enter text"
          rows="10"
          variant="outlined"
        />
      </v-col>
    </v-row>

    <v-row cols="12">
      <v-card>
        <v-card-title>TecAST Visualization</v-card-title>
        <v-card-text>
          <TecAST v-if="tecAST" :ast="tecAST" @deleted="handleRemove" />
          <span v-else class="text-grey">No AST to display...</span>
        </v-card-text>
      </v-card>
    </v-row>
    <v-row>
      <v-col cols="12" md="6">
        <v-card>
          <v-card-text>
            <strong>Current value:</strong> {{ appStore.textValue }}
          </v-card-text>
          <v-card-text>
            <strong>Debounced value (500ms):</strong> {{ debouncedValue }}
          </v-card-text>
        </v-card>
      </v-col>
    </v-row>
    <!-- Display the JSON result -->
    <v-row>
      <v-col cols="12" md="6">
        <v-card>
          <v-card-title>AST Json</v-card-title>
          <v-card-text>
            <v-progress-circular
              v-if="isLoading"
              color="primary"
              indeterminate
            />
            <pre
              v-else-if="jsonString"
              style="white-space: pre-wrap; word-break: break-word"
              >{{ jsonString }}</pre
            >
            <span v-else class="text-grey">No result yet...</span>
          </v-card-text>
        </v-card>
      </v-col>
    </v-row>
  </v-container>
</template>

<script lang="ts" setup>
import { useAppStore } from "@/stores/app";
import { refDebounced, useDebounceFn } from "@vueuse/core";
import { storeToRefs } from "pinia";
import { ref, watch } from "vue";
import { decodeTecAST, encodeTecAST, type TecAST as TecASTType, TecStr, TecType } from "@/schema/TecAstSchema.ts";

const appStore = useAppStore();

const debounceTime = 2000;
const jsonString = ref<string>("");
const isLoading = ref(false);
const tecAST = ref<TecASTType | null>(null);
function handleRemove() {
  console.log("removing root ast ...");
  tecAST.value = null;
  appStore.textValue = "";
}
async function formatCode() {
  const code = appStore.textValue;
  const formatted: string = await appStore.wasmInstance.formatHaskell(code);

  appStore.textValue = formatted
    .split("\n")
    .filter((line) => line.trim().length > 0)
    .join("\n");
}
watch(jsonString, (newValue) => {
  if (!newValue) {
    tecAST.value = null;
    return;
  }
  try {
    tecAST.value = decodeTecAST(JSON.parse(newValue));
    console.log(tecAST.value);
  } catch (error) {
    console.error("Error decoding TecAST:", error);
    tecAST.value = null;
  }
});

watch(jsonString, (newValue) => {
  if (newValue === null || newValue.length === 0) return;
  const tecAST = decodeTecAST(JSON.parse(newValue));
  console.log(tecAST);
});
// Option 1: Debounced callback function
const debouncedCallback = useDebounceFn(async (value: string) => {
  console.log("Debounced value changed:", value);

  if (!value) {
    jsonString.value = "";
    return;
  }

  try {
    isLoading.value = true;
    const jsonStringTask: Promise<string> =
      appStore.wasmInstance.parseHaskell(value);
    jsonString.value = await jsonStringTask;
  } catch (error) {
    console.error("Error parsing:", error);
    jsonString.value = `Error: ${error}`;
  } finally {
    isLoading.value = false;
  }
}, debounceTime);

watch(
  () => appStore.textValue,
  (newValue) => {
    debouncedCallback(newValue);
  },
);
function createHStack() {
  if (tecAST.value === null) {
    tecAST.value = TecType.make({
      typeName: "HStack",
      parameters: [
        TecType.make({
          typeName: "Text",
          parameters: [TecStr.make({ str: "(empty HStack)" })],
        }),
      ],
    });
  }
}
watch(tecAST, async (newValue) => {
  if (!newValue) return;
  const jsonStr = encodeTecAST(newValue).trim();
  console.log("converting ast back to haskell code ...", jsonStr);
  if (jsonStr) {
    const haskellCodeTask: Promise<string> =
      appStore.wasmInstance.makeHaskell(jsonStr);
    appStore.textValue = await haskellCodeTask;
  }
});
// Option 2: Debounced reactive ref (for display)
const { textValue } = storeToRefs(appStore);
const debouncedValue = refDebounced(textValue, debounceTime);
</script>
