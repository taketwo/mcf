return {
  {
    'folke/todo-comments.nvim',
    dependencies = {
      {
        'folke/which-key.nvim',
        opts = { defaults = { ['<Leader>t'] = { name = 'Trouble' } } },
      },
    },
    cmd = { 'TodoTrouble', 'TodoTelescope' },
    event = { 'BufReadPost', 'BufNewFile' },
    config = true,
    keys = {
      { ']t', function() require('todo-comments').jump_next() end, desc = 'Next todo comment' },
      { '[t', function() require('todo-comments').jump_prev() end, desc = 'Previous todo comment' },
      { '<leader>.t', '<cmd>TodoTelescope<cr>', desc = 'Show todo comments' },
      { '<leader>tt', '<cmd>Trouble todo toggle<cr>', desc = 'Show todo comments' },
      {
        '<leader>tT',
        '<cmd>Trouble todo toggle filter = {tag = {TODO,FIX,FIXME}}<cr>',
        desc = 'Show actionable todo comments',
      },
    },
  },
}
