local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
if not vim.uv.fs_stat(lazypath) then
  vim.fn.system({
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable', -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require('lazy').setup({
  spec = {
    -- We are only interested in the utility functions and built-in plugins that come with LazyVim.
    -- We want to be able to use them in our config and plugin specifications, thus LazyVim comes
    -- before importing our plugins.
    {
      'LazyVim/LazyVim',
      config = function()
        -- Not running the complete LazyVim setup as we are only interested in the utility functions
        _G.LazyVim = require('lazyvim.util')
      end,
      submodules = false, -- do not waste time cloning and updating submodules that we do not use
    },
    { import = 'plugins' },
  },
  diff = {
    cmd = 'git', -- May replace with "diffview.nvim" later
  },
  change_detection = {
    notify = false,
  },
  performance = {
    rtp = {
      -- Disable some rtp plugins
      disabled_plugins = {
        'gzip',
        -- "matchit",
        -- "matchparen",
        'netrw',
        'netrwPlugin',
        'tarPlugin',
        'tohtml',
        'tutor',
        'zipPlugin',
      },
    },
  },
  dev = {
    path = vim.fn.expand('$MCF') .. '/external/neovim-plugins',
    patterns = { 'taketwo' },
    fallback = true,
  },
})
