// Utilities
import { defineStore } from "pinia";
import { createGraphDB } from "@/graphdb.ts";

export interface HaskellWasm {
  makeHaskell(jsonStr: string): Promise<string>;
  parseHaskell(jsonStr: string): Promise<string>;
  formatHaskell(jsonStr: string): Promise<string>;
}
export const useAppStore = defineStore("app", {
  state: () => ({
    textValue: "",
    wasmInstance: null as HaskellWasm | null,
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
