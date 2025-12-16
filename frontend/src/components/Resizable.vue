<script lang="ts" setup>
import { ref } from "vue";

const containerRef = ref<HTMLElement | null>(null);
const isResizing = ref(false);
const resizeDirection = ref<"horizontal" | "vertical" | null>(null);
const containerWidth = ref<number | null>(null);
const containerHeight = ref<number | null>(null);
const isDragging = ref(false);
const containerLeft = ref<number | null>(null);
const containerTop = ref<number | null>(null);

const startResize = (
  direction: "horizontal" | "vertical",
  event: MouseEvent,
) => {
  event.preventDefault();
  event.stopPropagation();
  isResizing.value = true;
  resizeDirection.value = direction;

  const handleMouseMove = (e: MouseEvent) => {
    if (!containerRef.value || !isResizing.value) return;
    const rect = containerRef.value.getBoundingClientRect();

    if (resizeDirection.value === "horizontal") {
      const newWidth = e.clientX - rect.left;
      if (newWidth > 100) {
        containerWidth.value = newWidth;
      }
    } else if (resizeDirection.value === "vertical") {
      const newHeight = e.clientY - rect.top;
      if (newHeight > 100) {
        containerHeight.value = newHeight;
      }
    }
  };

  const handleMouseUp = () => {
    isResizing.value = false;
    resizeDirection.value = null;
    document.removeEventListener("mousemove", handleMouseMove);
    document.removeEventListener("mouseup", handleMouseUp);
  };

  document.addEventListener("mousemove", handleMouseMove);
  document.addEventListener("mouseup", handleMouseUp);
};

const startDrag = (event: MouseEvent) => {
  event.preventDefault();
  event.stopPropagation();
  isDragging.value = true;

  const startX = event.clientX;
  const startY = event.clientY;
  const rect = containerRef.value?.getBoundingClientRect();
  const initialLeft = rect?.left ?? 0;
  const initialTop = rect?.top ?? 0;

  const handleMouseMove = (e: MouseEvent) => {
    if (!isDragging.value) return;

    const deltaX = e.clientX - startX;
    const deltaY = e.clientY - startY;

    containerLeft.value = initialLeft + deltaX;
    containerTop.value = initialTop + deltaY;
  };

  const handleMouseUp = () => {
    isDragging.value = false;
    document.removeEventListener("mousemove", handleMouseMove);
    document.removeEventListener("mouseup", handleMouseUp);
  };

  document.addEventListener("mousemove", handleMouseMove);
  document.addEventListener("mouseup", handleMouseUp);
};
</script>

<template>
  <div
    ref="containerRef"
    :style="{
      width: containerWidth ? `${containerWidth}px` : 'auto',
      height: containerHeight ? `${containerHeight}px` : 'auto',
      position:
        containerLeft !== null || containerTop !== null ? 'fixed' : 'relative',
      left: containerLeft !== null ? `${containerLeft}px` : undefined,
      top: containerTop !== null ? `${containerTop}px` : undefined,
    }"
    class="resizable-container"
  >
    <!-- Drag handle at the top -->
    <div class="drag-handle" @mousedown="startDrag($event)"></div>

    <slot />

    <!-- Resize handles -->
    <div
      class="resize-handle resize-handle-horizontal"
      @mousedown="startResize('horizontal', $event)"
    ></div>
    <div
      class="resize-handle resize-handle-vertical"
      @mousedown="startResize('vertical', $event)"
    ></div>
  </div>
</template>

<style lang="sass" scoped>
.resizable-container
  min-width: 100px

.drag-handle
  position: absolute
  top: 0
  left: 50%
  transform: translateX(-50%)
  width: 80%
  max-width: 200px
  height: 6px
  background-color: rgba(243, 150, 33, 0.3)
  cursor: move
  transition: background-color 0.2s
  z-index: 11
  border-radius: 3px

  &:hover
    background-color: rgba(33, 150, 243, 0.8)

.resize-handle
  position: absolute
  background-color: rgba(33, 150, 243, 0.3)
  transition: background-color 0.2s
  z-index: 10

  &:hover
    background-color: rgba(33, 150, 243, 0.8)

.resize-handle-horizontal
  right: 0
  top: 50%
  transform: translateY(-50%)
  bottom: 0
  width: 3px
  height: 8px
  cursor: ew-resize

.resize-handle-vertical
  left: 50%
  transform: translateX(-50%)
  right: 0
  bottom: 0
  height: 3px
  width: 8px
  cursor: ns-resize
</style>
