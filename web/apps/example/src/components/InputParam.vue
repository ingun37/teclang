<script setup lang="ts">
import * as C from "codec";
import * as E from "effect";

const props = defineProps<{
  indexCombo: C.TecNode.IndexCombination;
  paramType: string;
  allEnums: C.TecEnum.TecEnumAST;
}>();
const model = defineModel<C.TecNode.TecNodeAttribute>({ required: true });
const inputValue = computed({
  get() {
    switch (model.value.tag) {
      case "TecNodeTextAttribute":
      case "TecNodeConAttribute":
        return model.value.contents;
      case "TecNodeIntAttribute":
        return model.value.contents.toString();
      case "TecNodeFracAttribute":
        return (
          model.value.contents.numerator / model.value.contents.denominator
        ).toString();
      default:
        return "";
    }
  },
  set(newValue: string) {
    function factory() {
      switch (props.paramType) {
        case "String":
        case "Image":
        case "Color":
          return C.TecNode.TecNodeTextAttribute.make({
            contents: newValue,
          });
        case "Number":
          const numValue = parseFloat(newValue);
          if (Number.isInteger(numValue)) {
            return C.TecNode.TecNodeIntAttribute.make({
              contents: numValue,
            });
          } else {
            // Handle float: extract numerator and denominator
            function gcd(a: number, b: number): number {
              return b === 0 ? a : gcd(b, a % b);
            }
            const decimalPart = newValue.split(".")[1] || "";
            let denominator = Math.pow(10, decimalPart.length);
            let numerator = Math.round(numValue * denominator);
            const divisor = gcd(Math.abs(numerator), Math.abs(denominator));
            numerator = numerator / divisor;
            denominator = denominator / divisor;
            return C.TecNode.TecNodeFracAttribute.make({
              contents: {
                denominator,
                numerator,
              },
            });
          }
        default:
          return C.TecNode.TecNodeConAttribute.make({
            contents: newValue,
          });
      }
    }
    model.value = factory();
  },
});

const onFileChange = (e: Event) => {
  const target = e.target as HTMLInputElement;
  const file = target.files?.[0];
  if (file) {
    inputValue.value = URL.createObjectURL(file);
  }
};

const clearImage = () => {
  inputValue.value = "";
};
function tecNodeIndexToStr(x: C.TecNode.TecNodeIndex): string {
  return x.tag === "TecNodeIndexWildcard" ? "_" : x.contents;
}
const label = computed(() => {
  return (
    props.indexCombo.map(tecNodeIndexToStr).join(", ") + " - " + props.paramType
  );
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
        v-model="inputValue"
        :label="label"
        variant="outlined"
        density="compact"
      />
    </template>
    <template v-else-if="paramType === 'Color'">
      <v-menu :close-on-content-click="false">
        <template v-slot:activator="{ props }">
          <v-btn
            v-bind="props"
            :color="inputValue"
            variant="flat"
            class="rounded border"
            size="small"
            width="100%"
          >
            {{ label }}
          </v-btn>
        </template>
        <v-color-picker v-model="inputValue" hide-inputs show-swatches />
      </v-menu>
    </template>
    <template v-else-if="paramType === 'Number'">
      <v-text-field
        v-model="inputValue"
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
          v-if="!inputValue"
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
            :src="inputValue"
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
        v-model="inputValue"
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
