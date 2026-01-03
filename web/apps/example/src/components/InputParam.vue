<script setup lang="ts">
import { ref } from "vue";
import * as C from "codec";
defineProps<{ paramType: C.TecClass.TecClassAttribute }>();

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

const clearImage = () => {
  imageUrl.value = null;
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
          v-if="!imageUrl"
          label="Upload Image"
          prepend-icon="mdi-camera"
          variant="outlined"
          density="compact"
          accept="image/*"
          hide-details
          @change="onFileChange"
        />
        <div v-else class="thumbnail-preview">
          <v-img
            :src="imageUrl"
            width="100"
            height="100"
            cover
            class="rounded border position-relative"
          >
            <v-btn
              icon="mdi-close"
              size="x-small"
              color="error"
              class="clear-btn"
              @click="clearImage"
            />
          </v-img>
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

.image-upload-container
  display: flex
  justify-content: center

.thumbnail-preview
  position: relative
  display: inline-block

.clear-btn
  position: absolute
  top: 4px
  right: 4px
  opacity: 0.8
  &:hover
    opacity: 1
</style>
