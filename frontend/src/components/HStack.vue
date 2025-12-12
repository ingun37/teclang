<script lang="ts" setup>
import TecAST from "@/components/TecAST.vue";
import type { TecAST as TecASTType } from "@/schema/TecAstSchema.ts";

interface Props {
  items: readonly TecASTType[];
}
const emit = defineEmits<{
  onItemRemove: [TecASTType, number];
  updated: [TecASTType, number];
}>();
const props = defineProps<Props>();
const showMenu = ref(false);

function deleteItem(item: TecASTType, index: number) {
  emit("onItemRemove", item, index);
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
  <v-sheet class="d-flex flex-row ga-1">
    <v-sheet v-for="(item, index) in items" :key="index">
      <Resizable>
        <v-spacer
          v-if="item.tag === 'TecType' && item.typeName === 'Spacing'"
          style="min-width: 100px"
        ></v-spacer>
        <TecAST
          v-else
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
        <v-list-item @click="handleAddItem">
          <v-list-item-title>Placeholder Item 2</v-list-item-title>
        </v-list-item>
        <v-list-item @click="handleAddItem">
          <v-list-item-title>Placeholder Item 3</v-list-item-title>
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
