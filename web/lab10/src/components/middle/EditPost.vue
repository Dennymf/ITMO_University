<template>
    <div class="form">
        <div class="header">Edit Post</div>
        <div class="body">
            <form @submit.prevent="onEdit">
                <div class="field">
                    <div class="name">
                        <label for="id">
                            ID:
                        </label>
                    </div>
                    <div class="value">
                        <input id="id" class="id" v-model="id"/>
                    </div>
                </div>
                <div class="field">
                    <div class="name">
                        <label for="text">
                            Text:
                        </label>
                    </div>
                    <div class="value">
                        <textarea id="text" v-model="text"></textarea>
                    </div>
                </div>
                <div class="error">{{ error }}</div>
                <div class="button-field">
                    <input type="submit" value="Edit"/>
                </div>
            </form>
        </div>
    </div>
</template>

<script>
export default {
    data: function () {
        return {
            id: "",
            text: "",
            error: ""
        }
    },
    name: "EditPost",
    beforeMount() {
        this.id = this.text = this.error = "";
        this.$root.$on("onEditPostValidationError", error => this.error = error);
    },
    methods: {
        onEdit: function () {
            this.$root.$emit("onEditPost", this.id, this.text);
        }
    }
}
</script>

<style scoped>
    label {
      display: block;
      margin-top: 1rem;
    }
    .id, textarea {
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
