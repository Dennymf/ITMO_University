<template>
    <header>
        <a href="#" @click.prevent="changePage('Index')">
            <img src="../assets/img/logo.png" alt="Codeforces" title="Codeforces"/>
        </a>
        <div class="languages">
            <a href="#"><img src="../assets/img/gb.png" alt="In English" title="In English"/></a>
            <a href="#"><img src="../assets/img/ru.png" alt="In Russian" title="In Russian"/></a>
        </div>
        <div class="enter-or-register-box">
            <template v-if="userId">
                Name: {{ users[userId].name }} Login: {{ users[userId].login }}
                |
                <a href="#page=Logout" @click.prevent="logout">Logout</a>
            </template>
            <template v-else>
                <a href="#page=Enter" @click.prevent="changePage('Enter')">Enter</a>
                |
              <a href="#page=Register" @click.prevent="changePage('Register')">Register</a>
            </template>
        </div>
        <nav>
            <ul>
                <li><a href="#page=Index" @click.prevent="changePage('Index')">Home</a></li>
                <li><a href="#page=Users" @click.prevent="changePage('Users')">Users</a></li>
                <li v-if="userId"><a href="#page=WritePost" @click.prevent="changePage('WritePost')">Write Post</a></li>
                <li v-if="userId"><a href="#page=EditPost" @click.prevent="changePage('EditPost')">Edit Post</a></li>
            </ul>
        </nav>
    </header>

</template>

<script>
export default {
    name: "Header",
    props: ["userId", "users"],
    beforeCreate() {
      this.$root.$on("onEnterSuccess", () => {
        this.changePage('Index');
      });

      this.$root.$on("onRegisterSuccess", () => {
        this.changePage('Enter');
      });
    },
    methods: {
      changePage: function (page) {
        this.$root.$emit("onChangePage", page);
      }, logout: function() {
        this.$root.$emit("onLogout");
        this.changePage('Index');
      }
    }
}
</script>

<style scoped>

</style>