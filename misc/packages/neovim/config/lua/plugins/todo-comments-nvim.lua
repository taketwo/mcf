return {
  {
    'folke/todo-comments.nvim',
    cmd = { 'TodoTrouble', 'TodoTelescope' },
    event = { 'BufReadPost', 'BufNewFile' },
    config = true,
    keys = {
      { ']t', function() require('todo-comments').jump_next() end, desc = 'Next todo comment' },
      { '[t', function() require('todo-comments').jump_prev() end, desc = 'Previous todo comment' },
      { '<Leader>.t', function() Snacks.picker.todo_comments() end, desc = 'Show todo comments' },
      {
        '<Leader>.T',
        function() Snacks.picker.todo_comments({ keywords = { 'TODO', 'FIX', 'FIXME' } }) end,
        desc = 'Show todo comments',
      },
      { '<Leader>tt', '<cmd>Trouble todo toggle<cr>', desc = 'Show todo comments' },
      {
        '<Leader>tT',
        '<cmd>Trouble todo toggle filter = {tag = {TODO,FIX,FIXME}}<cr>',
        desc = 'Show actionable todo comments',
      },
    },
  },
}
