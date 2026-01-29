<script setup lang="ts">
import * as C from "codec";
import * as E from "effect";
import TecNodesRecurse from "@/components/TecNodesRecurse.vue";

const model = defineModel<C.TecNode.TecNodeSet>({ required: true });

const props = defineProps<{
  tecEnums: C.TecEnum.TecEnumAST;
  tecClasses: C.TecClass.TecClassAST;
}>();
const enumOf = (
  pt: C.TecClass.TecClassIndex,
): E.Either.Either<C.TecEnum.TecEnum, Error> =>
  E.pipe(
    props.tecEnums.tecEnums,
    E.Array.findFirst((te) => (te.tecEnumName as string) === (pt as string)),
    E.Either.fromOption(() => new Error("Enum not found: " + pt)),
  );

type Eval = { dimension: string; tecClass: C.TecClass.TecClass };
const evaluation = computed<E.Either.Either<Eval, Error>>(() =>
  E.pipe(
    props.tecClasses.tecClasses,
    E.Array.findFirst((c) => c.tecClassName === model.value.tecNodeClass),
    E.Either.fromOption(
      () => new Error("Class not found: " + model.value.tecNodeClass),
    ),
    E.Either.andThen((tecClass: C.TecClass.TecClass) =>
      E.pipe(
        tecClass.tecSignature.indexTypeSet,
        E.Array.map(enumOf),
        E.Either.all,
        E.Either.map(
          (enums) =>
            `${enums.map((x) => x.tecEnumName + `(${x.tecEnumValues.length})`).join(" x ")}`,
        ),
        E.Either.map((dimension): Eval => ({ dimension, tecClass })),
      ),
    ),
  ),
);

const isUnique = ref(true);
</script>

<template>
  <v-alert
    v-if="E.Either.isLeft(evaluation)"
    :text="`${evaluation.left}`"
    title="Error"
    type="error"
  ></v-alert>
  <v-card v-else>
    <v-card-subtitle>
      <span>
        Input {{ model.tecNodeClass }} class data for each
        {{ evaluation.right.dimension }}
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
        :tec-indexed-class="evaluation.right.tecClass"
        :tec-enums="tecEnums"
      />
      <div v-else class="d-flex flex-column ga-1">
        <TecNode
          v-for="(_, i) in model.tecNodeSet"
          v-model="model.tecNodeSet[i]!"
          :enable-index-list-editing="true"
          :tec-indexed-class="evaluation.right.tecClass"
          :all-enums="tecEnums"
        ></TecNode>
      </div>
    </v-card-text>
  </v-card>
</template>

<style scoped lang="sass"></style>
