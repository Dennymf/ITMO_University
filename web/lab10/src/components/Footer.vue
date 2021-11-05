<template>
    <footer>
        <a href="/">Codeforces</a> &copy; 2020 by Mike Mirzayanov <br>
        Количество зарегистрированных пользователей: {{ users_count }} <br>
        Количество постов: {{ posts_count }}
    </footer>
</template>
<script>

export default {
    props: ['users_count', 'posts_count'],
    name: "Footer"
}
</script>

<style scoped>

</style>