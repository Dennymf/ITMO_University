<template>
  <section>
    <div class="header">
      <a :href="'#Post/' + post.id" @click="changePage('Post/{{post.id}}', post.id)">{{ post.title }}</a>
    </div>
    <div class="body">
      {{ post.text }}
    </div>
    <div class="footer">
      <a :href="'#Post/' + post.id" @click="changePage('Post/{{post.id}}', post.id)">View all</a>
    </div>
  </section>
</template>

<script>
export default {
  name: "SidebarPost",
  props: ["post"],
  methods: {
    changePage: function (page, id) {
      this.$root.$emit("onChangePage", page, id);
    }
  }
}
</script>

<style scoped>
section {
  margin-bottom: 1rem;
}
</style>