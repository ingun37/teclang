<script lang="ts" setup>
import type { StrictTableData } from "@/StrictTableData.ts";

defineProps<{ strictTable: StrictTableData }>();
</script>

<template>
  <v-table>
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
</template>

<style lang="sass" scoped></style>
