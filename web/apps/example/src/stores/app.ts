// Utilities
import { defineStore } from "pinia";
import type { HaskellEffect } from "@/teclang-wasm-effect.ts";
import type { Database } from "sql.js";
export const useAppStore = defineStore("app", {
  state: () => ({
    haskell: null as HaskellEffect | null,
    db: null as Database | null,
  }),
  actions: {
    setHaskell(he: HaskellEffect) {
      this.haskell = he;
    },
    setDB(db: Database) {
      this.db = db;
    },
  },
});
