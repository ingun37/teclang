<script lang="ts" setup>
import { TecAST, TecType } from "@/schema/TecAstSchema.ts";
import { decodeUnknownSync } from "effect/Schema";
import { RefinedTecType } from "@/schema/TecRefined.ts";
import Zip from "@/components/Zip.vue";
import Image from "@/components/Image.vue";
import { nonNull } from "@/nonnull.ts";

const props = defineProps<{ inputTecType: TecType }>();
const emit = defineEmits<{
  deleted: [TecType];
  updated: [TecType];
}>();

const refined = computed((): RefinedTecType | null => {
  try {
    return decodeUnknownSync(RefinedTecType)(props.inputTecType);
  } catch (e) {
    // console.warn("Error decoding TecType:", e);
    return null;
  }
});

function handleItemRemove(ast: TecAST, index: number) {
  console.log("removing", ast.tag);
  let newParams = [...props.inputTecType.parameters];
  newParams.splice(index, 1);

  if (newParams.length === 0) {
    console.log("After removing an item, HStack is empty. Removing it too ...");
    emit("deleted", props.inputTecType);
  } else
    emit(
      "updated",
      TecType.make({
        typeName: props.inputTecType.typeName,
        parameters: newParams,
      }),
    );
}
function handleItemUpdate(newItem: TecAST, index: number) {
  console.log("updating paramater", `#${index}`);

  let newParams = [...props.inputTecType.parameters];
  newParams[index] = newItem;
  emit(
    "updated",
    TecType.make({
      typeName: props.inputTecType.typeName,
      parameters: newParams,
    }),
  );
}
function handleItemAdd(newItem: TecAST) {
  let newParams = [...props.inputTecType.parameters].concat(newItem);
  emit(
    "updated",
    TecType.make({
      typeName: props.inputTecType.typeName,
      parameters: newParams,
    }),
  );
}
function handleItemReordered(x: number, y: number) {
  let newParams = [...props.inputTecType.parameters];
  newParams[x] = nonNull(props.inputTecType.parameters[y]);
  newParams[y] = nonNull(props.inputTecType.parameters[x]);
  emit(
    "updated",
    TecType.make({
      typeName: props.inputTecType.typeName,
      parameters: newParams,
    }),
  );
}
</script>

<template>
  <div>
    <v-sheet v-if="refined">
      <v-sheet v-if="refined.typeName === 'Text'">
        <Text :text="refined.parameters[0]!.str" />
      </v-sheet>
      <v-sheet v-else-if="refined.typeName === 'Pantone'">
        <Many :index-sets="refined.parameters" :type-name="refined.typeName" />
      </v-sheet>
      <v-sheet v-else-if="refined.typeName === 'Fabric'">
        <Fabrics :item="refined" />
      </v-sheet>
      <v-sheet v-else-if="refined.typeName === 'Render'">
        <Many :index-sets="refined.parameters" :type-name="refined.typeName" />
      </v-sheet>
      <v-sheet v-else-if="refined.typeName === 'Schematic'">
        <Many :index-sets="refined.parameters" :type-name="refined.typeName" />
      </v-sheet>
      <v-sheet v-else-if="refined.typeName === 'Image'">
        <Image :name="refined.parameters[0].str"></Image>
      </v-sheet>
      <v-sheet v-else-if="refined.typeName === 'Pom'">
        <Pom :pom="refined"></Pom>
      </v-sheet>
    </v-sheet>

    <v-sheet v-if="inputTecType.typeName === 'Logo'">
      <Logo />
    </v-sheet>
    <v-sheet v-if="inputTecType.typeName === 'Code'">
      <Code />
    </v-sheet>
    <v-sheet v-if="inputTecType.typeName === 'Name'">
      <Name />
    </v-sheet>
    <v-sheet v-if="inputTecType.typeName === 'PageNumber'">
      <PageNumber />
    </v-sheet>
    <v-sheet v-if="inputTecType.typeName === 'HStack'">
      <HStack
        :items="inputTecType.parameters"
        @added="(newItem) => handleItemAdd(newItem)"
        @onItemRemove="(item, index) => handleItemRemove(item, index)"
        @reordered="(x, y) => handleItemReordered(x, y)"
        @updated="(newItem, index) => handleItemUpdate(newItem, index)"
      />
    </v-sheet>
    <v-sheet v-if="inputTecType.typeName === 'VStack'">
      <VStack
        :items="inputTecType.parameters"
        @added="(newItem) => handleItemAdd(newItem)"
        @onItemRemove="(item, index) => handleItemRemove(item, index)"
        @reordered="(x, y) => handleItemReordered(x, y)"
        @updated="(newItem, index) => handleItemUpdate(newItem, index)"
      />
    </v-sheet>
    <v-sheet v-if="inputTecType.typeName === 'Zip'">
      <Zip :asts="inputTecType.parameters" />
    </v-sheet>
  </div>
</template>

<style lang="sass" scoped></style>
