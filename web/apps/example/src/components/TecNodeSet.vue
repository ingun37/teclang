<script setup lang="ts">
import * as C from "codec";
import * as E from "effect";
import TecNodesRecurse from "@/components/TecNodesRecurse.vue";

const model = defineModel<C.TecNode.TecNodeSet>({ required: true });

const props = defineProps<{
  tecEnums: C.TecEnum.TecEnumAST;
  tecClasses: C.TecClass.TecClassAST;
}>();

const tecClass = computed(() => {
  const found = E.Array.findFirst(
    props.tecClasses.tecClasses,
    (c) => c.tecClassName === model.value.tecNodeClass,
  );
  return E.Effect.runSync(
    E.Either.fromOption(
      found,
      () => new Error("Class not found: " + model.value.tecNodeClass),
    ),
  );
});
const dimension = computed(() => {
  const enums = E.pipe(
    tecClass.value.tecSignature.indexTypeSet,
    E.Array.filterMap((pt) => {
      return E.pipe(
        props.tecEnums.tecEnums,
        E.Array.findFirst(
          (te) => (te.tecEnumName as string) === (pt as string),
        ),
      );
    }),
  );
  const names = enums.map((x) => x.tecEnumName + `(${x.tecEnumValues.length})`);
  return `${names.join(" x ")}`;
});
const isUnique = ref(true);
</script>

<template>
  <v-card>
    <v-card-subtitle>
      <span>
        Input {{ model.tecNodeClass }} class data for each {{ dimension }}
      </span>

      <v-spacer />
      <v-checkbox
        v-model="isUnique"
        label="Grid"
        hide-details
        density="compact"
      />
    </v-card-subtitle>
    <v-card-text>
      <TecNodesRecurse
        v-if="isUnique"
        v-model="model.tecNodeSet"
        :index-combo="[]"
        axis="x"
        :param-index="0"
        :tec-indexed-class="tecClass"
        :tec-enums="tecEnums"
      />
      <div v-else class="d-flex flex-column ga-1">
        <TecNode
          v-for="(_, i) in model.tecNodeSet"
          v-model="model.tecNodeSet[i]!"
          :enable-index-list-editing="true"
          :tec-indexed-class="tecClass"
          :all-enums="tecEnums"
        ></TecNode>
      </div>
    </v-card-text>
  </v-card>
</template>

<style scoped lang="sass"></style>
