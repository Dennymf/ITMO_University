<template>
    <div class="middle">
        <Sidebar :posts="posts" :users="users"/>
        <main>
            <Index :users="users" :posts="posts" v-if="page === 'Index'"/>
            <Enter v-if="page === 'Enter'"/>
            <Register v-if="page === 'Register'"/>
            <WritePost v-if="page === 'WritePost'"/>
            <EditPost v-if="page === 'EditPost'"/>
            <Users :users="users" v-if="page === 'Users'"></Users>
            <FullPost :comments="comments" :users="users" :post="posts[id]" v-if="page.startsWith('Post/')"></FullPost>
        </main>
    </div>
</template>

<script>
  import FullPost from "@/components/middle/FullPost";
  import Users from "@/components/middle/Users";
  import Sidebar from "@/components/sidebar/Sidebar";
  import Index from "@/components/middle/Index";
  import Enter from "@/components/middle/Enter";
  import WritePost from "@/components/middle/WritePost";
  import EditPost from "@/components/middle/EditPost";
  import Register from "@/components/middle/Register";

export default {
    name: "Middle",
    props: ['users', 'posts', 'comments'],
    data: function () {
        return {
            page: "Index",
            id: null
        }
    },

    viewComments: function () {
      let temp = {};
      Object.values(this.comments).forEach( value => {
        if(value.postId in temp) {
          temp[value.postId]++;
        } else {
          temp[value.postId] = 1;
        }
      })
      return temp;
    },


    components: {
        FullPost,
        Users,
        EditPost,
        Index,
        Enter,
        Register,
        WritePost,
        Sidebar
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