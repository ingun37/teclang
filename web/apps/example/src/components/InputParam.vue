<script setup lang="ts">
import { ref } from "vue";

const props = defineProps<{ paramType: string }>();

const textValue = ref("");
const numberValue = ref<number | null>(null);
const imageUrl = ref<string | null>(null);

const onFileChange = (e: Event) => {
  const target = e.target as HTMLInputElement;
  const file = target.files?.[0];
  if (file) {
    imageUrl.value = URL.createObjectURL(file);
  }
};
</script>

<template>
  <div class="input-param">
    <template v-if="paramType === 'String'">
      <v-text-field
        v-model="textValue"
        label="String Input"
        variant="outlined"
        density="compact"
      />
    </template>

    <template v-else-if="paramType === 'Number'">
      <v-text-field
        v-model.number="numberValue"
        type="number"
        label="Number Input"
        variant="outlined"
        density="compact"
        hide-details
      />
    </template>

    <template v-else-if="paramType === 'Image'">
      <div class="image-upload-container">
        <v-file-input
          label="Upload Image"
          prepend-icon="mdi-camera"
          variant="outlined"
          density="compact"
          accept="image/*"
          @change="onFileChange"
        />
        <div v-if="imageUrl" class="thumbnail-preview mt-2">
          <v-img
            :src="imageUrl"
            width="100"
            height="100"
            cover
            class="rounded border"
          />
        </div>
      </div>
    </template>

    <template v-else>
      <span>{{ paramType }}</span>
    </template>
  </div>
</template>

<style scoped lang="sass">
.input-param
  padding: 8px
  min-width: 150px

.thumbnail-preview
  display: flex
  justify-content: center
</style>
