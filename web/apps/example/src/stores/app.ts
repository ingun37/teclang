// Utilities
import { defineStore } from "pinia";
import type { HaskellEffect } from "@/teclang-wasm-effect.ts";

export const useAppStore = defineStore("app", {
  state: () => ({
    haskell: null as HaskellEffect | null,
  }),
  actions: {
    setHaskell(he: HaskellEffect) {
      this.haskell = he;
    },
  },
});
