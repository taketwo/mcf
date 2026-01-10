return {
  {
    'nvim-mini/mini.snippets',
    event = 'InsertEnter',
    dependencies = 'rafamadriz/friendly-snippets',
    keys = {
      -- Expand or select snippet in normal mode
      {
        '<C-b>',
        function()
          if MiniSnippets.session.get() then
            MiniSnippets.session.stop()
            return
          end
          MiniSnippets.expand({ match = false })
        end,
        mode = { 'n' },
        desc = 'Select snippet to expand',
      },
      -- Expand snippet in insert mode (handled by mini.snippets)
      { '<C-b>', mode = { 'i' }, desc = 'Expand snippet' },
    },
    opts = function()
      local gen_loader = require('mini.snippets').gen_loader
      return {
        snippets = {
          gen_loader.from_lang({
            lang_patterns = {
              ['sh'] = { '**/bash.json' },
              ['gtest.cpp'] = { 'cpp/**/*.json' },
            },
          }),
        },
        mappings = {
          expand = '<C-b>',
          jump_next = '', -- handled by blink-cmp Tab mapping
          jump_prev = '', -- handled by blink-cmp S-Tab mapping
          stop = '', -- handled by normal mode <C-b> mapping
        },
      }
    end,
  },
}
