local fn = vim.fn
local install_path = fn.stdpath('data') .. '/site/pack/packer/opt/packer.nvim'
local compile_path = fn.stdpath('state') .. '/packer_compiled.lua'

if fn.empty(fn.glob(install_path)) > 0 then
  vim.api.nvim_set_hl(0, 'NormalFloat', { bg = '#1e222a' })
  print('Cloning packer...')
  fn.system({ 'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path })

  -- Install and compile plugins
  vim.cmd([[packadd packer.nvim]])
  require('plugins')
  require('packer').sync()

  -- Install language servers with Mason and Treesitter parsers
  vim.api.nvim_create_autocmd('User', {
    pattern = 'PackerComplete',
    callback = function()
      vim.cmd('bw | silent! MasonToolsInstall')
      require('packer').loader('nvim-treesitter')
    end,
  })
end
