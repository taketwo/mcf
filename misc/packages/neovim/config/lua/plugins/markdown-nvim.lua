return {
  'MeanderingProgrammer/markdown.nvim',
  enabled = false,
  ft = { 'markdown', 'norg', 'rmd', 'org' },
  opts = {
    file_types = { 'markdown', 'norg', 'rmd', 'org' },
    code = {
      sign = false,
      left_pad = 2,
    },
    heading = {
      sign = false,
      icons = {},
    },
    bullet = {
      icons = { '', '', '', '' },
    },
    callout = {
      todo = { raw = '[ ]', rendered = '󰄱', highlight = 'RenderMarkdownError' },
      done = { raw = '[+]', rendered = ' ', highlight = 'RenderMarkdownInfo' },
      cancelled = { raw = '[-]', rendered = ' ', highlight = 'RenderMarkdownWarn' },
      moved = { raw = '[~]', rendered = ' ', highlight = 'RenderMarkdownHint' },
    },
  },
  config = function(_, opts)
    require('render-markdown').setup(opts)
    LazyVim.toggle.map('<Leader>um', {
      name = 'markdown rendering',
      get = function() return require('render-markdown.state').enabled end,
      set = function(enabled)
        local m = require('render-markdown')
        if enabled then
          m.enable()
        else
          m.disable()
        end
      end,
    })
  end,
}
