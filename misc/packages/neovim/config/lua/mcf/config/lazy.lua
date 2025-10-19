local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
if not vim.uv.fs_stat(lazypath) then
  vim.fn.system({
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require('lazy').setup({
  spec = {
    -- We are interested in the utility functions and built-in plugins that come with LazyVim. We
    -- want to be able to use them in our config and plugin specifications, thus LazyVim comes
    -- before importing our plugins.
    {
      'LazyVim/LazyVim',
      -- By providing an explicit config function we avoid the complete LazyVim setup. Instead, we
      -- will only import the utils module and make it available in the global scope.
      config = function()
        _G.LazyVim = require('lazyvim.util')
        -- Commit https://github.com/LazyVim/LazyVim/commit/9c611b0c5758d0d659e7fdb29119b7083a56f989
        -- introduced a dependency between LazyVim utils and LazyVim config. Specifically, the
        -- set_default function uses the _options field from the config module. To avoid errors, we
        -- initialize this field in the same way as importing 'lazyvim.config' would. Somewhat
        -- confusingly, LazyVim stores the config module as a field in the LazyVim global (which is
        -- just an alias for 'lazyvim.util'), so we respect that here. Of coures, we do not import
        -- the actual config module, instead create a minimal stub table.
        _G.LazyVim.config = {
          _options = {
            indentexpr = vim.o.indentexpr,
            foldmethod = vim.o.foldmethod,
            foldexpr = vim.o.foldexpr,
          },
        }
      end,
      submodules = false, -- Do not waste time cloning and updating submodules that we do not use
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
