<template>
    <div class="middle">
        <Sidebar :posts="viewPosts"/>
        <main>
            <Index v-if="page === 'Index'" :posts="posts"/>
            <Users v-if="page === 'Users'" :users="users"/>
            <Enter v-if="page === 'Enter'"/>
            <Register v-if="page === 'Register'"/>
            <AddPost v-if="page === 'AddPost'"/>
            <FullPost :user="user" :post="viewedPost" v-if="page.startsWith('Post/')" />
        </main>
    </div>
</template>

<script>
import Sidebar from "@/components/sidebar/Sidebar";
import Index from "@/components/middle/Index";
import Enter from "@/components/middle/Enter";
import Register from "@/components/middle/Register";
import Users from "@/components/middle/Users";
import AddPost from "@/components/middle/AddPost";
import FullPost from "@/components/middle/FullPost";

export default {
    name: "Middle",
    data: function () {
        return {
            page: "Index"
        }
    },
    components: {
        Register,
        Enter,
        Index,
        Sidebar,
        Users,
        FullPost,
        AddPost
    },
    props: ["posts", "users", "user"],
    computed: {
        viewPosts: function () {
            return Object.values(this.posts).sort((a, b) => b.id - a.id).slice(0, 2);
        },
        viewedPost: function() {
          return this.posts.filter(p => p.id === this.id)[0];
        }
    }, beforeCreate() {
      this.$root.$on("onChangePage", (page, id) => {
        this.page = page;
        this.id = id;
      });
  }
}
</script>

<style scoped>

</style>