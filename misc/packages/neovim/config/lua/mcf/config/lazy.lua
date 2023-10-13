local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
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
    -- We want to be able to use them in our config and plugin specifications, thus LazyVime comes
    -- before importing our plugins. We also provide an empty setup function to prevent loading the
    -- entire LazyVim config.
    { 'LazyVim/LazyVim', config = function() end },
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
