<script lang="ts" setup>
import type { TecAST } from "@/schema/TecAST";
import Logo from "@/components/Logo.vue";
import Name from "@/components/Name.vue";
import PageNumber from "@/components/PageNumber.vue";
import HStack from "@/components/HStack.vue";
import VStack from "@/components/VStack.vue";
import Text from "@/components/Text.vue";
import Render from "@/components/Render.vue";

interface Props {
  ast: TecAST;
}

const props = defineProps<Props>();
</script>

<template>
  <v-card class="mb-2">
    <v-card-text>
      <v-sheet v-if="ast.tag === 'TecType'">
        <v-sheet v-if="ast.typeName === 'Logo'">
          <Logo />
        </v-sheet>
        <v-sheet v-if="ast.typeName === 'Code'">
          <Code />
        </v-sheet>
        <v-sheet v-if="ast.typeName === 'Name'">
          <Name />
        </v-sheet>
        <v-sheet v-if="ast.typeName === 'PageNumber'">
          <PageNumber />
        </v-sheet>
        <v-sheet v-if="ast.typeName === 'Text'">
          <v-sheet v-if="ast.index.tag === 'IndexS'">
            <Text :text="ast.index.name" />
          </v-sheet>
        </v-sheet>
        <v-sheet v-if="ast.typeName === 'Render'">
          <Render :item="ast" />
        </v-sheet>
        <v-sheet v-if="ast.typeName === 'Pantone'">
          <Pantone :item="ast" />
        </v-sheet>
      </v-sheet>

      <!-- TecLayout -->
      <v-sheet v-else-if="ast.tag === 'TecLayout'">
        <v-sheet v-if="ast.typeName === 'HStack'">
          <HStack :items="ast.children" />
        </v-sheet>
        <v-sheet v-if="ast.typeName === 'VStack'">
          <VStack :items="ast.children" />
        </v-sheet>
      </v-sheet>

      <!-- TecQuery -->
      <v-sheet v-else-if="ast.tag === 'TecQuery'">
        <Query :query="ast" />
      </v-sheet>
    </v-card-text>
  </v-card>
</template>

<style lang="sass" scoped></style>
