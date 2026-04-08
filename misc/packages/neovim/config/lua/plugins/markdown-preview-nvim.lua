return {
  'selimacerbas/markdown-preview.nvim',
  dependencies = {
    {
      'selimacerbas/live-server.nvim',
      cmd = { 'LiveServerStart', 'LiveServerStop' },
    },
  },
  cmd = { 'MarkdownPreview' },
  config = function() require('markdown_preview').setup({}) end,
}
