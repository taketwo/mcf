return {
  'LazyVim/LazyVim',
  config = function()
    -- Not running the complete LazyVim setup as we are only interested in the (patched) utility functions
    _G.LazyVim = require('mcf.config.lazyvim.util')
  end,
  submodules = false, -- do not waste time cloning and updating submodules that we do not use
}
