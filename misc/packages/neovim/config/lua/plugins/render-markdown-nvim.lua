return {
  'MeanderingProgrammer/render-markdown.nvim',
  commit = '617f9c2b', -- Pin to the last commit before TODO item support was refactored
  ft = { 'markdown', 'norg', 'rmd', 'org', 'codecompanion' },
  opts = {
    file_types = { 'markdown', 'norg', 'rmd', 'org', 'codecompanion' },
    render_modes = { 'n', 'c', 't', 'i' },
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
    checkbox = {
      custom = {
        todo = { raw = '[ ]', rendered = '[ ]', highlight = 'RenderMarkdownError', scope_highlight = nil },
        done = { raw = '[+]', rendered = '[+]', highlight = 'RenderMarkdownInfo', scope_highlight = nil },
        cancelled = { raw = '[-]', rendered = '[-]', highlight = 'RenderMarkdownWarn', scope_highlight = nil },
        moved = { raw = '[~]', rendered = '[~]', highlight = 'RenderMarkdownHint', scope_highlight = nil },
      },
    },
  },
  config = function(_, opts)
    require('render-markdown').setup(opts)
    Snacks.toggle({
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
    }):map('<Leader>um')
  end,
}
