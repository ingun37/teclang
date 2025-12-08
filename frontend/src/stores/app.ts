// Utilities
import { defineStore } from "pinia";

export const useAppStore = defineStore("app", {
  state: () => ({
    textValue: "",
    wasmInstance: null as any,
  }),
  actions: {
    updateTextValue(value: string) {
      this.textValue = value;
    },
    setWasmInstance(exports: any) {
      this.wasmInstance = exports;
    },
  },
});
