<template>
  <div>
    <MiddlePost v-for="post in allPosts"
                :post="post" :key="post.id"/>
  </div>
</template>

<script>
import MiddlePost from "./MiddlePost";

export default {
  name: "Index",
  components: {MiddlePost},
  props: ['posts'],
  computed: {
    allPosts: function() {
      return Object.values(this.posts).sort((p1, p2) => p2.id - p1.id);
    },
  }
}
</script>

<style scoped>

</style>