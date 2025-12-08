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

    <!-- Display the throttled value -->
    <v-row>
      <v-col cols="12" md="6">
        <v-card>
          <v-card-text>
            <strong>Current value:</strong> {{ appStore.textValue }}
          </v-card-text>
          <v-card-text>
            <strong>Throttled value (500ms):</strong> {{ throttledValue }}
          </v-card-text>
        </v-card>
      </v-col>
    </v-row>
  </v-container>
</template>

<script lang="ts" setup>
import { useAppStore } from "@/stores/app";
import { refThrottled, useThrottleFn } from "@vueuse/core";
import { watch } from "vue";
// Or use storeToRefs + computed for a throttled reactive value
import { storeToRefs } from "pinia";

const appStore = useAppStore();
const throttleTime = 2000;
// Subscribe to throttled changes
const throttledCallback = useThrottleFn((value: string) => {
  console.log("Throttled value changed:", value);
  // Do something with the throttled value
  // e.g., API call, expensive computation, etc.
}, throttleTime);

watch(
  () => appStore.textValue,
  (newValue) => {
    throttledCallback(newValue);
  },
);

const { textValue } = storeToRefs(appStore);
const throttledValue = refThrottled(textValue, throttleTime);
</script>
