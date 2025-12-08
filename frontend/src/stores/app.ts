// Utilities
import { defineStore } from "pinia";

export const useAppStore = defineStore("app", {
  state: () => ({
    textValue: "",
  }),
  actions: {
    updateTextValue(value: string) {
      this.textValue = value;
    },
  },
});
