<!-- Sample Vue SFC for lexer benchmarking -->
<template>
  <div class="bench" :class="{ active: isActive }">
    <h1>{{ title }}</h1>
    <ul v-if="items.length > 0">
      <li v-for="(item, i) in items" :key="i" @click="select(i)">
        {{ item.name }}: <strong>{{ item.score.toFixed(3) }}</strong>
      </li>
    </ul>
    <p v-else>No items yet.</p>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue'

const title = ref('Benchmark')
const isActive = ref(false)
const items = ref([
  { name: 'first', score: 1.5 },
  { name: 'second', score: 2.75 },
])

const total = computed(() => items.value.reduce((a, e) => a + e.score, 0))

function select(i) {
  isActive.value = true
  console.log('selected', i, total.value)
}
</script>

<style scoped>
.bench { padding: 16px; }
.bench.active { border: 1px solid #336699; }
strong { color: #ff8800; }
</style>

<!-- Sample Vue SFC for lexer benchmarking -->
<template>
  <div class="bench" :class="{ active: isActive }">
    <h1>{{ title }}</h1>
    <ul v-if="items.length > 0">
      <li v-for="(item, i) in items" :key="i" @click="select(i)">
        {{ item.name }}: <strong>{{ item.score.toFixed(3) }}</strong>
      </li>
    </ul>
    <p v-else>No items yet.</p>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue'

const title = ref('Benchmark')
const isActive = ref(false)
const items = ref([
  { name: 'first', score: 1.5 },
  { name: 'second', score: 2.75 },
])

const total = computed(() => items.value.reduce((a, e) => a + e.score, 0))

function select(i) {
  isActive.value = true
  console.log('selected', i, total.value)
}
</script>

<style scoped>
.bench { padding: 16px; }
.bench.active { border: 1px solid #336699; }
strong { color: #ff8800; }
</style>

<!-- Sample Vue SFC for lexer benchmarking -->
<template>
  <div class="bench" :class="{ active: isActive }">
    <h1>{{ title }}</h1>
    <ul v-if="items.length > 0">
      <li v-for="(item, i) in items" :key="i" @click="select(i)">
        {{ item.name }}: <strong>{{ item.score.toFixed(3) }}</strong>
      </li>
    </ul>
    <p v-else>No items yet.</p>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue'

const title = ref('Benchmark')
const isActive = ref(false)
const items = ref([
  { name: 'first', score: 1.5 },
  { name: 'second', score: 2.75 },
])

const total = computed(() => items.value.reduce((a, e) => a + e.score, 0))

function select(i) {
  isActive.value = true
  console.log('selected', i, total.value)
}
</script>

<style scoped>
.bench { padding: 16px; }
.bench.active { border: 1px solid #336699; }
strong { color: #ff8800; }
</style>

<!-- Sample Vue SFC for lexer benchmarking -->
<template>
  <div class="bench" :class="{ active: isActive }">
    <h1>{{ title }}</h1>
    <ul v-if="items.length > 0">
      <li v-for="(item, i) in items" :key="i" @click="select(i)">
        {{ item.name }}: <strong>{{ item.score.toFixed(3) }}</strong>
      </li>
    </ul>
    <p v-else>No items yet.</p>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue'

const title = ref('Benchmark')
const isActive = ref(false)
const items = ref([
  { name: 'first', score: 1.5 },
  { name: 'second', score: 2.75 },
])

const total = computed(() => items.value.reduce((a, e) => a + e.score, 0))

function select(i) {
  isActive.value = true
  console.log('selected', i, total.value)
}
</script>

<style scoped>
.bench { padding: 16px; }
.bench.active { border: 1px solid #336699; }
strong { color: #ff8800; }
</style>

<!-- Sample Vue SFC for lexer benchmarking -->
<template>
  <div class="bench" :class="{ active: isActive }">
    <h1>{{ title }}</h1>
    <ul v-if="items.length > 0">
      <li v-for="(item, i) in items" :key="i" @click="select(i)">
        {{ item.name }}: <strong>{{ item.score.toFixed(3) }}</strong>
      </li>
    </ul>
    <p v-else>No items yet.</p>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue'

const title = ref('Benchmark')
const isActive = ref(false)
const items = ref([
  { name: 'first', score: 1.5 },
  { name: 'second', score: 2.75 },
])

const total = computed(() => items.value.reduce((a, e) => a + e.score, 0))

function select(i) {
  isActive.value = true
  console.log('selected', i, total.value)
}
</script>

<style scoped>
.bench { padding: 16px; }
.bench.active { border: 1px solid #336699; }
strong { color: #ff8800; }
</style>

<!-- Sample Vue SFC for lexer benchmarking -->
<template>
  <div class="bench" :class="{ active: isActive }">
    <h1>{{ title }}</h1>
    <ul v-if="items.length > 0">
      <li v-for="(item, i) in items" :key="i" @click="select(i)">
        {{ item.name }}: <strong>{{ item.score.toFixed(3) }}</strong>
      </li>
    </ul>
    <p v-else>No items yet.</p>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue'

const title = ref('Benchmark')
const isActive = ref(false)
const items = ref([
  { name: 'first', score: 1.5 },
  { name: 'second', score: 2.75 },
])

const total = computed(() => items.value.reduce((a, e) => a + e.score, 0))

function select(i) {
  isActive.value = true
  console.log('selected', i, total.value)
}
</script>

<style scoped>
.bench { padding: 16px; }
.bench.active { border: 1px solid #336699; }
strong { color: #ff8800; }
</style>

<!-- Sample Vue SFC for lexer benchmarking -->
<template>
  <div class="bench" :class="{ active: isActive }">
    <h1>{{ title }}</h1>
    <ul v-if="items.length > 0">
      <li v-for="(item, i) in items" :key="i" @click="select(i)">
        {{ item.name }}: <strong>{{ item.score.toFixed(3) }}</strong>
      </li>
    </ul>
    <p v-else>No items yet.</p>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue'

const title = ref('Benchmark')
const isActive = ref(false)
const items = ref([
  { name: 'first', score: 1.5 },
  { name: 'second', score: 2.75 },
])

const total = computed(() => items.value.reduce((a, e) => a + e.score, 0))

function select(i) {
  isActive.value = true
  console.log('selected', i, total.value)
}
</script>

<style scoped>
.bench { padding: 16px; }
.bench.active { border: 1px solid #336699; }
strong { color: #ff8800; }
</style>

<!-- Sample Vue SFC for lexer benchmarking -->
<template>
  <div class="bench" :class="{ active: isActive }">
    <h1>{{ title }}</h1>
    <ul v-if="items.length > 0">
      <li v-for="(item, i) in items" :key="i" @click="select(i)">
        {{ item.name }}: <strong>{{ item.score.toFixed(3) }}</strong>
      </li>
    </ul>
    <p v-else>No items yet.</p>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue'

const title = ref('Benchmark')
const isActive = ref(false)
const items = ref([
  { name: 'first', score: 1.5 },
  { name: 'second', score: 2.75 },
])

const total = computed(() => items.value.reduce((a, e) => a + e.score, 0))

function select(i) {
  isActive.value = true
  console.log('selected', i, total.value)
}
</script>

<style scoped>
.bench { padding: 16px; }
.bench.active { border: 1px solid #336699; }
strong { color: #ff8800; }
</style>

<!-- Sample Vue SFC for lexer benchmarking -->
<template>
  <div class="bench" :class="{ active: isActive }">
    <h1>{{ title }}</h1>
    <ul v-if="items.length > 0">
      <li v-for="(item, i) in items" :key="i" @click="select(i)">
        {{ item.name }}: <strong>{{ item.score.toFixed(3) }}</strong>
      </li>
    </ul>
    <p v-else>No items yet.</p>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue'

const title = ref('Benchmark')
const isActive = ref(false)
const items = ref([
  { name: 'first', score: 1.5 },
  { name: 'second', score: 2.75 },
])

const total = computed(() => items.value.reduce((a, e) => a + e.score, 0))

function select(i) {
  isActive.value = true
  console.log('selected', i, total.value)
}
</script>

<style scoped>
.bench { padding: 16px; }
.bench.active { border: 1px solid #336699; }
strong { color: #ff8800; }
</style>

<!-- Sample Vue SFC for lexer benchmarking -->
<template>
  <div class="bench" :class="{ active: isActive }">
    <h1>{{ title }}</h1>
    <ul v-if="items.length > 0">
      <li v-for="(item, i) in items" :key="i" @click="select(i)">
        {{ item.name }}: <strong>{{ item.score.toFixed(3) }}</strong>
      </li>
    </ul>
    <p v-else>No items yet.</p>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue'

const title = ref('Benchmark')
const isActive = ref(false)
const items = ref([
  { name: 'first', score: 1.5 },
  { name: 'second', score: 2.75 },
])

const total = computed(() => items.value.reduce((a, e) => a + e.score, 0))

function select(i) {
  isActive.value = true
  console.log('selected', i, total.value)
}
</script>

<style scoped>
.bench { padding: 16px; }
.bench.active { border: 1px solid #336699; }
strong { color: #ff8800; }
</style>

<!-- Sample Vue SFC for lexer benchmarking -->
<template>
  <div class="bench" :class="{ active: isActive }">
    <h1>{{ title }}</h1>
    <ul v-if="items.length > 0">
      <li v-for="(item, i) in items" :key="i" @click="select(i)">
        {{ item.name }}: <strong>{{ item.score.toFixed(3) }}</strong>
      </li>
    </ul>
    <p v-else>No items yet.</p>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue'

const title = ref('Benchmark')
const isActive = ref(false)
const items = ref([
  { name: 'first', score: 1.5 },
  { name: 'second', score: 2.75 },
])

const total = computed(() => items.value.reduce((a, e) => a + e.score, 0))

function select(i) {
  isActive.value = true
  console.log('selected', i, total.value)
}
</script>

<style scoped>
.bench { padding: 16px; }
.bench.active { border: 1px solid #336699; }
strong { color: #ff8800; }
</style>

<!-- Sample Vue SFC for lexer benchmarking -->
<template>
  <div class="bench" :class="{ active: isActive }">
    <h1>{{ title }}</h1>
    <ul v-if="items.length > 0">
      <li v-for="(item, i) in items" :key="i" @click="select(i)">
        {{ item.name }}: <strong>{{ item.score.toFixed(3) }}</strong>
      </li>
    </ul>
    <p v-else>No items yet.</p>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue'

const title = ref('Benchmark')
const isActive = ref(false)
const items = ref([
  { name: 'first', score: 1.5 },
  { name: 'second', score: 2.75 },
])

const total = computed(() => items.value.reduce((a, e) => a + e.score, 0))

function select(i) {
  isActive.value = true
  console.log('selected', i, total.value)
}
</script>

<style scoped>
.bench { padding: 16px; }
.bench.active { border: 1px solid #336699; }
strong { color: #ff8800; }
</style>

<!-- Sample Vue SFC for lexer benchmarking -->
<template>
  <div class="bench" :class="{ active: isActive }">
    <h1>{{ title }}</h1>
    <ul v-if="items.length > 0">
      <li v-for="(item, i) in items" :key="i" @click="select(i)">
        {{ item.name }}: <strong>{{ item.score.toFixed(3) }}</strong>
      </li>
    </ul>
    <p v-else>No items yet.</p>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue'

const title = ref('Benchmark')
const isActive = ref(false)
const items = ref([
  { name: 'first', score: 1.5 },
  { name: 'second', score: 2.75 },
])

const total = computed(() => items.value.reduce((a, e) => a + e.score, 0))

function select(i) {
  isActive.value = true
  console.log('selected', i, total.value)
}
</script>

<style scoped>
.bench { padding: 16px; }
.bench.active { border: 1px solid #336699; }
strong { color: #ff8800; }
</style>

<!-- Sample Vue SFC for lexer benchmarking -->
<template>
  <div class="bench" :class="{ active: isActive }">
    <h1>{{ title }}</h1>
    <ul v-if="items.length > 0">
      <li v-for="(item, i) in items" :key="i" @click="select(i)">
        {{ item.name }}: <strong>{{ item.score.toFixed(3) }}</strong>
      </li>
    </ul>
    <p v-else>No items yet.</p>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue'

const title = ref('Benchmark')
const isActive = ref(false)
const items = ref([
  { name: 'first', score: 1.5 },
  { name: 'second', score: 2.75 },
])

const total = computed(() => items.value.reduce((a, e) => a + e.score, 0))

function select(i) {
  isActive.value = true
  console.log('selected', i, total.value)
}
</script>

<style scoped>
.bench { padding: 16px; }
.bench.active { border: 1px solid #336699; }
strong { color: #ff8800; }
</style>

