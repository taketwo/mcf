local test_dir = vim.fn.stdpath('config') .. '/tests'
vim.opt.runtimepath:append(test_dir)
vim.cmd([[let &rtp.=','.getcwd()]])

local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable',
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require('lazy').setup({
  {
    'nvim-mini/mini.test',
    config = function()
      require('mini.test').setup({
        collect = {
          find_files = function() return vim.fn.globpath('tests', '**/*_spec.lua', true, true) end,
        },
      })
    end,
  },
})
