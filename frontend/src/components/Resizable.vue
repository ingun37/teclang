<script lang="ts" setup>
import { ref } from "vue";

const containerRef = ref<HTMLElement | null>(null);
const isResizing = ref(false);
const resizeDirection = ref<"horizontal" | "vertical" | null>(null);
const containerWidth = ref<number | null>(null);
const containerHeight = ref<number | null>(null);

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
</script>

<template>
  <div
    ref="containerRef"
    :style="{
      width: containerWidth ? `${containerWidth}px` : 'auto',
      height: containerHeight ? `${containerHeight}px` : 'auto',
      position: 'relative',
    }"
    class="resizable-container"
  >
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
