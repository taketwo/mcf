return {
  {
    'w0rp/ale',
    lazy = false,
    keys = {
      { '<F2>', '<cmd>ALEFix<cr>', desc = 'Apply ALE fixers' },
    },
    init = function()
      vim.g.ale_sign_column_always = 1
      vim.g.ale_sign_error = '✸'
      vim.g.ale_sign_warning = '✶'
      vim.g.ale_sign_style_error = '✤'
      vim.g.ale_sign_style_warning = '✢'
      vim.g.ale_completion_enabled = 0 -- Disable completion (language servers will do that)
      vim.g.ale_use_neovim_diagnostics_api = 1 -- Send linter output to diagnostics API
    end,
  },
}
