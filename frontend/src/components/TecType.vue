<script lang="ts" setup>
import { TecAST, TecASTEquivalence, TecType } from "@/schema/TecAstSchema.ts";
import { decodeUnknownSync } from "effect/Schema";
import { RefinedTecType } from "@/schema/TecRefined.ts";
import Zip from "@/components/Zip.vue";
import Image from "@/components/Image.vue";

const props = defineProps<{ inputTecType: TecType }>();
const emit = defineEmits<{
  deleted: [TecType];
}>();

const tecType = ref(props.inputTecType);

const refined = computed((): RefinedTecType | null => {
  try {
    return decodeUnknownSync(RefinedTecType)(tecType.value);
  } catch (e) {
    console.warn("Error decoding TecType:", e);
    return null;
  }
});

const isSelected = ref(false);

const handleClick = () => {
  isSelected.value = true;
};

const handleDeleteKey = (event: KeyboardEvent) => {
  if (event.key === "Delete" || event.key === "Backspace") {
    console.log("delete me", tecType.value.typeName);
    emit("deleted", tecType.value);
  }
};

watch(isSelected, (selected) => {
  if (selected) {
    window.addEventListener("keydown", handleDeleteKey);
  } else {
    window.removeEventListener("keydown", handleDeleteKey);
  }
});

onBeforeUnmount(() => {
  window.removeEventListener("keydown", handleDeleteKey);
});

function handleItemRemove(ast: TecAST) {
  console.log("removing", ast.tag);
  const newParams = tecType.value.parameters.filter(
    (x) => !TecASTEquivalence(ast, x),
  );

  if (newParams.length === 0) {
    console.log("After removing an item, HStack is empty. Removing it too ...");
    emit("deleted", props.inputTecType);
  } else
    tecType.value = TecType.make({
      typeName: tecType.value.typeName,
      parameters: newParams,
    });
}
</script>

<template>
  <div @click.stop="handleClick">
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

    <v-sheet v-if="tecType.typeName === 'Logo'">
      <Logo />
    </v-sheet>
    <v-sheet v-if="tecType.typeName === 'Code'">
      <Code />
    </v-sheet>
    <v-sheet v-if="tecType.typeName === 'Name'">
      <Name />
    </v-sheet>
    <v-sheet v-if="tecType.typeName === 'PageNumber'">
      <PageNumber />
    </v-sheet>
    <v-sheet v-if="tecType.typeName === 'HStack'">
      <HStack :items="tecType.parameters" @onItemRemove="handleItemRemove" />
    </v-sheet>
    <v-sheet v-if="tecType.typeName === 'VStack'">
      <VStack :items="tecType.parameters" />
    </v-sheet>
    <v-sheet v-if="tecType.typeName === 'Zip'">
      <Zip :asts="tecType.parameters" />
    </v-sheet>
  </div>
</template>

<style lang="sass" scoped></style>
