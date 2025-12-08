<script lang="ts" setup>
import type { TecAST } from "@/schema/TecAST";

interface Props {
  ast: TecAST;
}

const props = defineProps<Props>();
</script>

<template>
  <v-card class="mb-2">
    <v-card-title class="text-caption">
      {{ ast.tag }}
    </v-card-title>
    <v-card-text>
      <!-- TecType -->
      <div v-if="ast.tag === 'TecType'">
        <div><strong>Type Name:</strong> {{ ast.typeName }}</div>
        <div><strong>Index:</strong> {{ ast.index.tag }}</div>
        <div v-if="ast.index.tag === 'IndexN'">
          <strong>Number:</strong> {{ ast.index.number }}
        </div>
        <div v-if="ast.index.tag === 'IndexS'">
          <strong>Name:</strong> {{ ast.index.name }}
        </div>
      </div>

      <!-- TecLayout -->
      <div v-else-if="ast.tag === 'TecLayout'">
        <div><strong>Type Name:</strong> {{ ast.typeName }}</div>
        <div v-if="ast.children.length > 0" class="mt-2">
          <strong>Children:</strong>
          <div class="ml-4 mt-2">
            <TecAST
              v-for="(child, index) in ast.children"
              :key="index"
              :ast="child"
            />
          </div>
        </div>
      </div>

      <!-- TecQuery -->
      <div v-else-if="ast.tag === 'TecQuery'">
        <div><strong>Operator:</strong> {{ ast.op }}</div>
        <div class="mt-2">
          <strong>Left:</strong>
          <div class="ml-4 mt-2">
            <TecAST :ast="ast.left" />
          </div>
        </div>
        <div class="mt-2">
          <strong>Right:</strong>
          <div class="ml-4 mt-2">
            <TecAST :ast="ast.right" />
          </div>
        </div>
      </div>
    </v-card-text>
  </v-card>
</template>

<style lang="sass" scoped>
.ml-4
  margin-left: 1rem

.mt-2
  margin-top: 0.5rem

.mb-2
  margin-bottom: 0.5rem
</style>
