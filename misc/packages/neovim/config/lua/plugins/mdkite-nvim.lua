return {
  'selimacerbas/mdkite.nvim',
  dependencies = {
    {
      'selimacerbas/kitehost.nvim',
      cmd = { 'KiteHost' },
    },
  },
  cmd = { 'MdKite' },
  config = function()
    require('mdkite').setup({
      hooks = {
        on_start = function(url) vim.notify('Preview started: ' .. url, vim.log.levels.INFO) end,
        on_stop = function() vim.notify('Preview stopped', vim.log.levels.INFO) end,
      },
    })
  end,
}
