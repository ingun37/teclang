<script lang="ts" setup>
import TecAST from "@/components/TecAST.vue";
import { type TecAST as TecASTType, TecBinding, TecStr, TecType } from "@/schema/TecAstSchema.ts";
import SelectTecType from "@/components/SelectTecType.vue";

interface Props {
  items: readonly TecASTType[];
  axis: "X" | "Y";
}
const emit = defineEmits<{
  removed: [TecASTType, number];
  updated: [TecASTType, number];
  added: [TecASTType];
  reordered: [number, number]; // [fromIndex, toIndex]
}>();
const props = defineProps<Props>();
const showMenu = ref(false);
const showTypeDialog = ref(false);

// Drag and drop state
const draggedIndex = ref<number | null>(null);
const dragOverIndex = ref<number | null>(null);

const flexDirection = computed(() =>
  props.axis === "Y" ? "flex-column" : "flex-row",
);
const girth = "1em";
const buttonDimensions = computed(() =>
  props.axis === "Y"
    ? { width: "100%", height: girth, minHeight: girth, minWidth: undefined }
    : { height: "100%", width: girth, minWidth: girth },
);
type Selection =
  | "Text"
  | "Vertical Stack"
  | "Horizontal Stack"
  | "Binding"
  | "Query";
const topSelections: Selection[] = [
  "Text",
  "Vertical Stack",
  "Horizontal Stack",
  "Binding",
  "Query",
];
function deleteItem(item: TecASTType, index: number) {
  emit("removed", item, index);
}
function updateItem(newItem: TecASTType, index: number) {
  emit("updated", newItem, index);
}
function handleAddItem(topS: Selection) {
  if (topS === "Query") {
    showTypeDialog.value = true;
    return;
  }
  function makeTec(): TecASTType {
    switch (topS) {
      case "Text":
        return TecStr.make({ str: "(place holder)" });
      case "Vertical Stack":
        return TecType.make({
          typeName: "VStack",
          parameters: [TecStr.make({ str: "(empty v-stack)" })],
        });
      case "Horizontal Stack":
        return TecType.make({
          typeName: "VStack",
          parameters: [TecStr.make({ str: "(empty h-stack)" })],
        });
      case "Binding":
        return TecBinding.make({
          varMap: {},
          expression: TecStr.make({ str: "(empty binding)" }),
        });
      default:
        throw new Error(`Unknown top selection: ${topS}`);
    }
  }

  showMenu.value = false;

  emit("added", makeTec());
}

// Drag and drop handlers
function handleDragStart(index: number) {
  draggedIndex.value = index;
}

function handleDragOver(event: DragEvent, index: number) {
  event.preventDefault();
  dragOverIndex.value = index;
}

function handleDragLeave() {
  dragOverIndex.value = null;
}

function handleDrop(event: DragEvent, targetIndex: number) {
  event.preventDefault();
  if (draggedIndex.value !== null && draggedIndex.value !== targetIndex) {
    emit("reordered", draggedIndex.value, targetIndex);
  }
  draggedIndex.value = null;
  dragOverIndex.value = null;
}

function handleDragEnd() {
  draggedIndex.value = null;
  dragOverIndex.value = null;
}
</script>

<template>
  <v-sheet :class="['d-flex', flexDirection, 'ga-1']">
    <v-sheet
      v-for="(item, index) in items"
      :key="index"
      :class="['item-container', { 'drag-over': dragOverIndex === index }]"
      @dragleave="handleDragLeave"
      @dragover="(e) => handleDragOver(e, index)"
      @drop="(e) => handleDrop(e, index)"
    >
      <div class="d-flex align-center">
        <div
          class="drag-handle"
          draggable="true"
          @dragend="handleDragEnd"
          @dragstart="handleDragStart(index)"
        >
          <v-icon size="small">mdi-drag-vertical</v-icon>
        </div>
        <Resizable>
          <TecAST
            :ast="item"
            @deleted="deleteItem(item, index)"
            @updated="(newItem) => updateItem(newItem, index)"
          />
        </Resizable>
      </div>
    </v-sheet>

    <v-menu v-model="showMenu" location="bottom">
      <template v-slot:activator="{ props }">
        <v-btn
          :height="buttonDimensions.height"
          :min-height="buttonDimensions.minHeight"
          :min-width="buttonDimensions.minWidth"
          :width="buttonDimensions.width"
          class="add-button"
          v-bind="props"
          variant="text"
        >
          +
        </v-btn>
      </template>
      <v-list>
        <v-list-item v-for="s in topSelections" @click="() => handleAddItem(s)">
          <v-list-item-title>{{ s }}</v-list-item-title>
        </v-list-item>
      </v-list>
    </v-menu>

    <v-dialog v-model="showTypeDialog">
      <v-card>
        <v-card-title>Select Type</v-card-title>
        <v-card-text>
          <SelectTecType
            @returned="
              (ast) => {
                emit('added', ast);
                showTypeDialog = false;
              }
            "
          />
        </v-card-text>
        <v-card-actions>
          <v-spacer />
          <v-btn @click="showTypeDialog = false">Cancel</v-btn>
        </v-card-actions>
      </v-card>
    </v-dialog>
  </v-sheet>
</template>

<style lang="sass" scoped>

.add-button
  padding: 0 !important
  font-size: 18px
  align-self: stretch
  background-color: #e0ed7a

.item-container
  position: relative
  transition: background-color 0.2s

  &.drag-over
    background-color: rgba(0, 0, 0, 0.05)

.drag-handle
  cursor: grab
  padding: 4px
  display: flex
  align-items: center
  user-select: none
  color: #666

  &:active
    cursor: grabbing

  &:hover
    background-color: rgba(0, 0, 0, 0.05)
    border-radius: 4px
</style>
