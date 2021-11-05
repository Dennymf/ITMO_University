<template>
    <section>
        <div class="header">
            {{ post.title }}
        </div>
        <div class="body">
            {{ post.text }}
        </div>
        <div class="footer">
        <a href="#" @click.prevent="changePage('Post/' + post.id, post.id)">View all</a>
        </div>
    </section>
</template>

<script>
export default {
  props: ['post'],
  name: "SidebarPost",
  methods: {
    changePage: function(page, id) {
      this.$root.$emit("onChangePage", page, id)
    }
  }
}
</script>

<style scoped>
section {
    margin-bottom: 1rem;
}
</style>