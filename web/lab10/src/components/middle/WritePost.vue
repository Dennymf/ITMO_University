<template>
    <div class="form">
        <div class="header">Write Post</div>
        <div class="body">
            <form @submit.prevent="onWritePost">
                <div class="field">
                    <div class="name">
                        <label for="title">Title:</label>
                    </div>
                    <div class="value">
                        <input id="title" name="title" v-model="title"/>
                    </div>
                </div>
                <div class="field">
                    <div class="name">
                        <label for="text">Text:</label>
                    </div>
                    <div class="value">
                        <textarea id="text" name="text" v-model="text"></textarea>
                    </div>
                </div>
                <div class="error">{{ error }}</div>
                <div class="button-field">
                    <input type="submit" value="Write">
                </div>
            </form>
        </div>
    </div>
</template>

<script>
export default {
    name: "WritePost",
    data: function () {
        return {
            title: "",
            text: "",
            error: ""
        }
    },
    methods: {
        onWritePost: function () {
            this.error = "";
            this.$root.$emit("onWritePost", this.title, this.text);
        }
    },
    beforeMount() {
        this.title = this.text = this.error = "";
        this.$root.$on("onWritePostValidationError", error => this.error = error);
    }
}
</script>

<style scoped>
  label {
    display: block;
    margin-top: 1rem;
  }
  .title, textarea {
    width: 60%;
    box-sizing: border-box;
  }
  input[type='submit'] {
    margin-top: 1rem;
    width: 6rem;
  }
  .error {
    color: var(--error-color);
  }
</style>