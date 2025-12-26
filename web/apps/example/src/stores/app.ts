// Utilities
import { defineStore } from "pinia";

export interface HaskellWasm {
  makeHaskell(jsonStr: string): Promise<string>;
  parseHaskell(jsonStr: string): Promise<string>;
  formatHaskell(jsonStr: string): Promise<string>;
}
export const useAppStore = defineStore("app", {
  state: () => ({
    wasmInstance: null as HaskellWasm | null,
  }),
  actions: {
    setWasmInstance(exports: any) {
      this.wasmInstance = exports;
    },
  },
});
