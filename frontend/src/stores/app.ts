// Utilities
import { defineStore } from "pinia";
import { createGraphDB } from "@/graphdb.ts";

export const useAppStore = defineStore("app", {
  state: () => ({
    textValue: "",
    wasmInstance: null as any,
    graphDB: createGraphDB(),
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
