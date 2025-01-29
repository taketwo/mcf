-- Set up package path
local test_dir = vim.fn.stdpath('config') .. '/tests'
vim.opt.runtimepath:append(test_dir)
vim.cmd([[let &rtp.=','.getcwd()]])

-- Bootstrap lazy.nvim
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

-- Load minimal plugin spec for tests
require('lazy').setup({
  {
    'echasnovski/mini.test',
    config = function()
      require('mini.test').setup({
        collect = {
          find_files = function() return vim.fn.globpath('tests', '**/*_spec.lua', true, true) end,
          -- silent = true,
        },
      })
      -- Move the setup here
      -- local MiniTest = require('mini.test')
      -- MiniTest.new({
      --   root = vim.fn.stdpath('config') .. '/tests',
      --   find_files = function()
      --     return vim.fn.globpath(vim.fn.stdpath('config') .. '/tests', '**/*_spec.lua', false, true)
      --   end,
      -- })
    end,
  },
  -- Add other plugins your tests depend on
})
