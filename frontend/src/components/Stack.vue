<script lang="ts" setup>
import TecAST from "@/components/TecAST.vue";
import {
  type TecAST as TecASTType,
  TecInt,
  TecStr,
  type TecType,
} from "@/schema/TecAstSchema.ts";
import SelectTecType from "@/components/SelectTecType.vue";

interface Props {
  items: readonly TecASTType[];
  axis: "X" | "Y";
}
const emit = defineEmits<{
  removed: [TecASTType, number];
  updated: [TecASTType, number];
  added: [TecASTType];
}>();
const props = defineProps<Props>();
const showMenu = ref(false);
const showTypeDialog = ref(false);

const flexDirection = computed(() =>
  props.axis === "Y" ? "flex-column" : "flex-row",
);
const girth = "1em";
const buttonDimensions = computed(() =>
  props.axis === "Y"
    ? { width: "100%", height: girth, minHeight: girth, minWidth: undefined }
    : { height: "100%", width: girth, minWidth: girth },
);
const topSelections: TecASTType["tag"][] = [
  "TecInt",
  "TecStr",
  "TecRngInt",
  "TecVar",
  "TecBinding",
  "TecType",
  "TecList",
  "TecQuery",
];
function deleteItem(item: TecASTType, index: number) {
  emit("removed", item, index);
}
function updateItem(newItem: TecASTType, index: number) {
  emit("updated", newItem, index);
}
function handleAddItem(topS: TecASTType["tag"]) {
  if (topS === "TecType") {
    showTypeDialog.value = true;
    return;
  }
  function makeTec(): TecASTType {
    switch (topS) {
      case "TecInt":
        return TecInt.make({ int: 0 });
      case "TecStr":
        return TecStr.make({ str: "(place holder)" });
      default:
        throw new Error(`Unknown top selection: ${topS}`);
    }
  }

  showMenu.value = false;

  emit("added", makeTec());
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

    <v-dialog v-model="showTypeDialog" max-width="400">
      <v-card>
        <v-card-title>Select Type</v-card-title>
        <v-card-text>
          <SelectTecType />
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
</style>
