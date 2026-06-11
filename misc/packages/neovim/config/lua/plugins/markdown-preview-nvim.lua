return {
  'selimacerbas/markdown-preview.nvim',
  dependencies = {
    {
      'selimacerbas/live-server.nvim',
      cmd = { 'LiveServerStart', 'LiveServerStop' },
    },
  },
  cmd = { 'MarkdownPreview' },
  config = function()
    require('markdown_preview').setup({
      hooks = {
        on_start = function(url) vim.notify('Preview started: ' .. url, vim.log.levels.INFO) end,
        on_stop = function() vim.notify('Preview stopped', vim.log.levels.INFO) end,
      },
    })
  end,
}
