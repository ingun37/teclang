<script setup lang="ts">
import { ref } from "vue";
import * as C from "codec";
import * as E from "effect";
const props = defineProps<{
  indexCombo: string[];
  paramType: string;
  allEnums: C.TecEnum.TecEnumAST;
}>();

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

const label = computed(() => {
  return props.indexCombo.join(", ");
});

const options = computed<readonly string[]>(() => {
  return E.Effect.runSync(
    E.pipe(
      props.allEnums.tecEnums,
      E.Array.findFirst(
        (tecEnum) => (tecEnum.tecEnumName as string) === props.paramType,
      ),
      E.Either.fromOption(() => new Error("Failed to find enum with the type")),
      E.Either.map((foundEnum) => foundEnum.tecEnumValues),
    ),
  );
});
</script>
<template>
  <div class="input-param">
    <template v-if="paramType === 'String'">
      <v-text-field
        v-model="textValue"
        :label="label"
        variant="outlined"
        density="compact"
      />
    </template>

    <template v-else-if="paramType === 'Number'">
      <v-text-field
        v-model.number="numberValue"
        type="number"
        :label="label"
        variant="outlined"
        density="compact"
        hide-details
      />
    </template>

    <template v-else-if="paramType === 'Image'">
      <div class="image-upload-container">
        <v-file-input
          v-if="!imageUrl"
          :label="label"
          prepend-icon="mdi-camera"
          variant="outlined"
          density="compact"
          accept="image/*"
          hide-details
          @change="onFileChange"
          style="width: 9rem"
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
      <v-select
        :items="options"
        density="compact"
        hide-details
        variant="outlined"
        style="width: 8rem"
        :label="label"
      />
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
