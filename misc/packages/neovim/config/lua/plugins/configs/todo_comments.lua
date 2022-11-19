local todo_comments = require('todo-comments')

todo_comments.setup{}

vim.keymap.set("n", "]t", todo_comments.jump_next, { desc = "Next todo comment" })
vim.keymap.set("n", "[t", todo_comments.jump_prev, { desc = "Previous todo comment" })
