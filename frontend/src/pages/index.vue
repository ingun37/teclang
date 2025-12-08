<template>
  <v-container>
    <v-row>
      <v-col cols="12" md="6">
        <v-text-field
          v-model="appStore.textValue"
          clearable
          label="Enter text"
          variant="outlined"
        />
      </v-col>
    </v-row>

    <!-- Display the debounced value -->
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
  </v-container>
</template>

<script lang="ts" setup>
import { useAppStore } from "@/stores/app";
import { refDebounced, useDebounceFn } from "@vueuse/core";
import { storeToRefs } from "pinia";
import { watch } from "vue";

const appStore = useAppStore();

const debounceTime = 2000;
// Option 1: Debounced callback function
const debouncedCallback = useDebounceFn((value: string) => {
  console.log("Debounced value changed:", value);
  // Do something with the debounced value
  // e.g., API call, search query, etc.
}, debounceTime);

watch(
  () => appStore.textValue,
  (newValue) => {
    debouncedCallback(newValue);
  },
);

// Option 2: Debounced reactive ref (for display)
const { textValue } = storeToRefs(appStore);
const debouncedValue = refDebounced(textValue, debounceTime);
</script>
