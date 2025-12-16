<script lang="ts" setup>
import { ref } from "vue";

const props = defineProps<{ labels: string[] }>();

const emit = defineEmits<{
  reorder: [fromIndex: number, toIndex: number];
}>();

const draggedIndex = ref<number | null>(null);
const dragOverIndex = ref<number | null>(null);

const handleDragStart = (index: number) => {
  draggedIndex.value = index;
};

const handleDragOver = (event: DragEvent, index: number) => {
  event.preventDefault();
  dragOverIndex.value = index;
};

const handleDragLeave = () => {
  dragOverIndex.value = null;
};

const handleDrop = (event: DragEvent, dropIndex: number) => {
  event.preventDefault();

  if (draggedIndex.value === null || draggedIndex.value === dropIndex) {
    draggedIndex.value = null;
    dragOverIndex.value = null;
    return;
  }

  const fromIndex = draggedIndex.value;

  emit("reorder", fromIndex, dropIndex);

  draggedIndex.value = null;
  dragOverIndex.value = null;
};

const handleDragEnd = () => {
  draggedIndex.value = null;
  dragOverIndex.value = null;
};
</script>

<template>
  <v-sheet class="reorder-container">
    <v-chip
      v-for="(label, index) in labels"
      :key="`${label}-${index}`"
      :class="{
        dragging: draggedIndex === index,
        'drag-over': dragOverIndex === index,
      }"
      :draggable="true"
      class="reorder-chip"
      size="small"
      @dragend="handleDragEnd"
      @dragleave="handleDragLeave"
      @dragover="handleDragOver($event, index)"
      @dragstart="handleDragStart(index)"
      @drop="handleDrop($event, index)"
    >
      <v-icon size="x-small" start>mdi-drag-horizontal-variant</v-icon>
      {{ label }}
    </v-chip>
  </v-sheet>
</template>

<style lang="sass" scoped>
.reorder-container
  display: flex
  flex-direction: row
  gap: 8px
  align-items: center
  padding: 8px
  flex-wrap: wrap

.reorder-chip
  cursor: grab
  transition: all 0.2s ease
  user-select: none

  &:hover
    transform: translateY(-2px)
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15)

  &.dragging
    opacity: 0.4
    cursor: grabbing

  &.drag-over
    transform: scale(1.1)
    box-shadow: 0 0 0 2px rgb(var(--v-theme-primary))
</style>
