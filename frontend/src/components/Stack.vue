<script lang="ts" setup>
import TecAST from "@/components/TecAST.vue";
import type { TecAST as TecASTType } from "@/schema/TecAstSchema.ts";

interface Props {
  items: readonly TecASTType[];
  axis: "X" | "Y";
}
const emit = defineEmits<{
  removed: [TecASTType, number];
  updated: [TecASTType, number];
}>();
const props = defineProps<Props>();
const showMenu = ref(false);
const flexDirection = computed(() =>
  props.axis === "Y" ? "flex-column" : "flex-row",
);

const buttonDimensions = computed(() =>
  props.axis === "Y"
    ? { width: "100%", height: "8px", minHeight: "8px", minWidth: undefined }
    : { height: "100%", width: "8px", minWidth: "8px" },
);

function deleteItem(item: TecASTType, index: number) {
  emit("removed", item, index);
}
function updateItem(newItem: TecASTType, index: number) {
  emit("updated", newItem, index);
}
function handleAddItem() {
  // Placeholder action
  console.log("Add item clicked");
  showMenu.value = false;
}
</script>

<template>
  <v-sheet :class="['d-flex', flexDirection, 'ga-1']">
    <v-sheet v-for="(item, index) in items" :key="index">
      <Resizable>
        <TecAST
          :ast="item"
          @deleted="deleteItem(item, index)"
          @updated="(newItem) => updateItem(newItem, index)"
        />
      </Resizable>
    </v-sheet>

    <v-menu v-model="showMenu" location="bottom">
      <template v-slot:activator="{ props }">
        <v-btn
          class="add-button"
          height="100%"
          min-width="8px"
          v-bind="props"
          variant="text"
          width="8px"
        >
          +
        </v-btn>
      </template>
      <v-list>
        <v-list-item @click="handleAddItem">
          <v-list-item-title>Placeholder Item 1</v-list-item-title>
        </v-list-item>
      </v-list>
    </v-menu>
  </v-sheet>
</template>

<style lang="sass" scoped>

.add-button
  padding: 0 !important
  font-size: 18px
  align-self: stretch
  background-color: #e0ed7a
</style>
