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
type RNE<T> = E.Array.NonEmptyReadonlyArray<T>;
type EV = C.TecEnum.TecEnumValue;
type Eval = {
  dimension: string;
  tecClass: C.TecClass.TecClass;
  missingCombos: readonly (readonly string[])[];
};
const iterateClass = C.help.iterateIndexSet(props.tecEnums);

const evaluation = computed<E.Either.Either<Eval, Error>>(() =>
  E.Either.gen(function* () {
    const tecClass: C.TecClass.TecClass = yield* E.pipe(
      props.tecClasses.tecClasses,
      E.Array.findFirst((c) => c.tecClassName === model.value.tecNodeClass),
      E.Either.fromOption(
        () => new Error("Class not found: " + model.value.tecNodeClass),
      ),
    );
    const dimension = yield* E.pipe(
      tecClass.tecSignature.indexTypeSet,
      E.Array.map(enumOf),
      E.Either.all,
      E.Either.map(
        (enums) =>
          `${enums.map((x) => x.tecEnumName + `(${x.tecEnumValues.length})`).join(" x ")}`,
      ),
    );

    const iterateCombo = C.help.iterateIndexCombo(
      props.tecEnums,
      tecClass.tecSignature.indexTypeSet,
    );
    type RNE<T> = E.Array.NonEmptyReadonlyArray<T>;
    type EV = C.TecEnum.TecEnumValue;
    const eq = E.Array.getEquivalence(E.Equivalence.string);
    const ord = E.Array.getOrder(E.Order.string);
    const allCombos: RNE<RNE<EV>> = yield* iterateClass(
      tecClass.tecSignature.indexTypeSet,
    );

    const currentCombos: RNE<RNE<EV>> = yield* E.pipe(
      model.value.tecNodeSet,
      E.Array.map((tn) => iterateCombo(tn.indexCombination)),
      E.Either.all,
      E.Either.map(E.Array.flatten),
    );

    const currentCombos_ = E.pipe(
      currentCombos,
      E.Array.sort(ord),
      E.Array.dedupeAdjacentWith(eq),
    );
    const missingCombos = E.Array.differenceWith(eq)(
      E.pipe(allCombos, E.Array.sort(ord)),
      currentCombos_,
    );

    return {
      dimension,
      tecClass,
      missingCombos,
    };
  }),
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
      <v-container fluid>
        <v-row dense>
          <v-col cols="8">
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
          </v-col>
          <v-col cols="4"> {{ evaluation.right.missingCombos }} </v-col>
        </v-row>
      </v-container>
    </v-card-text>
  </v-card>
</template>

<style scoped lang="sass"></style>
