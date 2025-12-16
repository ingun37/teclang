<script lang="ts" setup>
import type { TecAST } from "@/schema/TecAstSchema.ts";
import TecType from "@/components/TecType.vue";

interface Props {
  ast: TecAST;
}

const props = defineProps<Props>();
const emit = defineEmits<{
  deleted: [TecAST];
  updated: [TecAST];
}>();
function onDelete() {
  console.log("Delete");
  emit("deleted", props.ast);
}

const isSelected = ref(false);
const showMenu = ref(false);
const menuX = ref(0);
const menuY = ref(0);

const handleClick = () => {
  isSelected.value = !isSelected.value;
};

const handleContextMenu = (event: MouseEvent) => {
  if (isSelected.value) {
    event.preventDefault();
    showMenu.value = false;
    menuX.value = event.clientX;
    menuY.value = event.clientY;
    nextTick(() => {
      showMenu.value = true;
    });
  }
};
</script>

<template>
  <div
    :class="{ 'selected-tectype': isSelected }"
    @contextmenu="handleContextMenu"
    @click.stop="handleClick"
  >
    <v-sheet v-if="ast.tag === 'TecType'">
      <TecType
        :input-tec-type="ast"
        @deleted="onDelete"
        @updated="(x) => emit('updated', x)"
      />
    </v-sheet>
    <v-sheet v-if="ast.tag === 'TecQuery'">
      <Query :query="ast" @updated="(x) => emit('updated', x)" />
    </v-sheet>
    <v-sheet v-if="ast.tag === 'TecBinding'">
      <TecBinding :binding="ast" @updated="(x) => emit('updated', x)" />
    </v-sheet>
    <v-sheet v-if="ast.tag === 'TecVar'"> variable:{{ ast.varName }} </v-sheet>
    <v-sheet v-if="ast.tag === 'TecInt'">{{ ast.int }}</v-sheet>
    <v-sheet v-if="ast.tag === 'TecStr'">{{ ast.str }}</v-sheet>

    <v-menu
      v-model="showMenu"
      :style="`position: fixed; left: ${menuX}px; top: ${menuY}px;`"
      absolute
    >
      <v-list>
        <v-list-item @click="onDelete">
          <v-list-item-title>Delete</v-list-item-title>
        </v-list-item>
      </v-list>
    </v-menu>
  </div>
</template>

<style lang="sass" scoped>


.selected-tectype
  outline: 2px solid #1976d2
  outline-offset: 2px
  background-color: rgba(25, 118, 210, 0.08)
  border-radius: 4px
</style>
