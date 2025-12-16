<script lang="ts" setup>
import type { StrictTableData } from "@/StrictTableData.ts";
import { transpose } from "@/functions.ts";

const props = defineProps<{ strictTable: StrictTableData }>();
const isTransposed = ref(false);
const transposedTails = computed(() => transpose(props.strictTable.tails));
</script>

<template>
  <div>
    <v-switch
      v-model="isTransposed"
      class="mb-4"
      color="primary"
      hide-details
      label="Transpose Table"
    ></v-switch>

    <v-table v-if="!isTransposed">
      <tbody>
        <tr v-for="(tail, index) in strictTable.tails" :key="index">
          <td>
            <Single :t-entry="strictTable.headers[index]!" />
          </td>
          <td v-for="(entry, index2) in tail" :key="index2">
            <Single :t-entry="entry"></Single>
          </td>
        </tr>
      </tbody>
    </v-table>
    <v-table v-else>
      <thead>
        <tr>
          <th v-for="h in strictTable.headers">
            <Single :t-entry="h"></Single>
          </th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="(tail, i) in transposedTails" :key="i">
          <td v-for="(entry, j) in tail" :key="j">
            <Single :t-entry="entry"></Single>/>
          </td>
        </tr>
      </tbody>
    </v-table>
  </div>
</template>

<style lang="sass" scoped></style>
