local plugins = {
  ['wbthomason/packer.nvim'] = {
    cmd = { 'PackerStatus', 'PackerSync', 'PackerInstall', 'PackerUpdate', 'PackerClean', 'PackerCompile' },
    config = function() require('plugins') end,
  },

  ['nvim-lua/plenary.nvim'] = { module = 'plenary' },

  ['norcalli/nvim-colorizer.lua'] = {
    cmd = 'ColorizerToggle',
    config = function() require('colorizer').setup() end,
  },

  ['andymass/vim-matchup'] = {},
  ['godlygeek/tabular'] = {},
  ['AndrewRadev/splitjoin.vim'] = {},
  ['navarasu/onedark.nvim'] = {},
  ['nvim-lualine/lualine.nvim'] = {
    requires = { 'nvim-tree/nvim-web-devicons' },
  },
  ['romgrk/barbar.nvim'] = {
    requires = { 'nvim-tree/nvim-web-devicons' },
  },
  ['kshenoy/vim-signature'] = {},
  ['tmsvg/pear-tree'] = {},
  ['scrooloose/nerdcommenter'] = {},
  ['w0rp/ale'] = {},
  ['SirVer/ultisnips'] = {},
  ['kylechui/nvim-surround'] = {},
  ['embear/vim-localvimrc'] = {},
  ['dantler/vim-alternate'] = {},
  ['tpope/vim-eunuch'] = {},
  ['tpope/vim-dispatch'] = {},
  ['Peeja/vim-cdo'] = {},
  ['tpope/vim-abolish'] = {},
  ['mbbill/undotree'] = {
    cmd = { 'UndotreeToggle' },
  },
  ['xolox/vim-misc'] = {},
  ['taketwo/vim-exchange'] = {},
  ['janko-m/vim-test'] = {},
  ['tweekmonster/startuptime.vim'] = {},
  ['sickill/vim-pasta'] = {},
  ['taketwo/vimux'] = {},
  ['jeffkreeftmeijer/vim-numbertoggle'] = {},
  ['taketwo/diffchar.vim'] = {},
  ['AndrewRadev/deleft.vim'] = {},

  -- Navigation
  ['justinmk/vim-sneak'] = {},
  ['christoomey/vim-tmux-navigator'] = {},
  ['pechorin/any-jump.vim'] = {},

  -- Search
  ['henrik/vim-indexed-search'] = {},
  ['nelstrom/vim-visual-star-search'] = {},
  ['dyng/ctrlsf.vim'] = {},

  ------------------------------------------------------------------------------
  --                         Languages and filetypes                          --
  ------------------------------------------------------------------------------

  ['preservim/vim-markdown'] = {
    ft = { 'markdown ' },
  },
  ['jkramer/vim-checkbox'] = {
    ft = { 'markdown' },
  },
  ['tikhomirov/vim-glsl'] = {},
  ['lervag/vimtex'] = {
    ft = { 'tex' },
  },
  ['keith/tmux.vim'] = {},
  ['LnL7/vim-nix'] = {},
  ['hjson/vim-hjson'] = {},

  ----------------------------------------------------------------------------
  --                      Frameworks and environments                       --
  ----------------------------------------------------------------------------

  ['folke/neodev.nvim'] = {},
  ['taketwo/vim-ros'] = {
    -- branch = 'cmp'
  },

  ------------------------------------------------------------------------------
  --                             Language servers                             --
  ------------------------------------------------------------------------------

  ['williamboman/mason.nvim'] = {
    config = function() require('mason').setup() end,
  },

  ['WhoIsSethDaniel/mason-tool-installer.nvim'] = {
    after = 'mason.nvim',
  },

  ['williamboman/mason-lspconfig.nvim'] = {
    after = 'mason.nvim',
    config = function() require('mason-lspconfig').setup() end,
  },

  ['neovim/nvim-lspconfig'] = {
    after = { 'mason-lspconfig.nvim', 'neodev.nvim' },
  },

  ['glepnir/lspsaga.nvim'] = {
    config = function() require('lspsaga').init_lsp_saga() end,
  },

  ['onsails/lspkind-nvim'] = {},

  -- Git related
  ['tpope/vim-fugitive'] = {},
  ['tpope/vim-git'] = {},
  ['lewis6991/gitsigns.nvim'] = {},
  ['junegunn/gv.vim'] = {},
  ['whiteinge/diffconflicts'] = {},
  ['rhysd/git-messenger.vim'] = {
    cmd = { 'GitMessenger' },
  },
  ['jreybert/vimagit'] = {
    cmd = { 'Magit' },
  },
  ['ruifm/gitlinker.nvim'] = {
    requires = { 'nvim-lua/plenary.nvim' },
  },

  -- Text objects
  ['michaeljsmith/vim-indent-object'] = {},
  ['kana/vim-textobj-entire'] = {
    requires = { 'kana/vim-textobj-user' },
  },
  ['jceb/vim-textobj-uri'] = {
    requires = { 'kana/vim-textobj-user' },
  },
  ['nvim-treesitter/nvim-treesitter-textobjects'] = {},

  -- Snippets
  ['honza/vim-snippets'] = {},
  ['kkoomen/vim-doge'] = {},

  -- Misc
  ['folke/which-key.nvim'] = { module = 'which-key' },
  ['voldikss/vim-floaterm'] = {},

  -- Misc unsorted
  ['kevinhwang91/rnvimr'] = {},
  ['nvim-treesitter/nvim-treesitter'] = {
    run = function() pcall(require('nvim-treesitter.install').update({ with_sync = true })) end,
  },
  ['nvim-treesitter/playground'] = {
    -- cmd = {'TSHighlightCapturesUnderCursor'}
  },
  ['nvim-telescope/telescope.nvim'] = {
    requires = { { 'nvim-lua/popup.nvim' }, { 'nvim-lua/plenary.nvim' } },
  },
  ['nvim-telescope/telescope-fzf-native.nvim'] = {
    run = 'make',
  },
  ['nvim-telescope/telescope-symbols.nvim'] = {},
  ['nvim-telescope/telescope-ui-select.nvim'] = {},
  ['lukas-reineke/indent-blankline.nvim'] = {},
  ['folke/todo-comments.nvim'] = {},
  ['nvim-tree/nvim-tree.lua'] = {
    requires = { 'nvim-tree/nvim-web-devicons' },
    cmd = { 'NvimTreeToggle', 'NvimTreeOpen' },
    config = function() require('nvim-tree').setup() end,
  },
  ['folke/trouble.nvim'] = {
    requires = { 'nvim-tree/nvim-web-devicons' },
    cmd = { 'TroubleToggle', 'Trouble', 'TroubleRefresh', 'TroubleClose' },
    module = 'trouble',
  },

  -- Autocompletion
  ['hrsh7th/nvim-cmp'] = {},
  ['hrsh7th/cmp-nvim-lsp'] = {},
  ['hrsh7th/cmp-buffer'] = {},
  ['hrsh7th/cmp-omni'] = {},
  ['hrsh7th/cmp-path'] = {},
  ['quangnguyen30192/cmp-nvim-ultisnips'] = {},
  ['zbirenbaum/copilot.lua'] = {
    event = 'VimEnter',
  },
}

local process_plugins = function(plugins)
  local final_table = {}
  for key, val in pairs(plugins) do
    local plugin_name = require('utils').extract_plugin_name(key)
    if val and type(val) == 'table' then
      local config_dir = vim.fn.stdpath('config') .. '/lua/plugins/config'
      local config_file = string.format('%s/%s.lua', config_dir, plugin_name)
      if vim.fn.filereadable(config_file) == 1 then
        if not val.config then
          val.config =
            function(name) require(string.format('plugins.config.%s', require('utils').extract_plugin_name(name))) end
        else
          print(
            string.format(
              'WARNING: plugin %s has both config file and config option, the latter will be used',
              plugin_name
            )
          )
        end
      end
      local setup_dir = vim.fn.stdpath('config') .. '/lua/plugins/setup'
      local setup_file = string.format('%s/%s.lua', setup_dir, plugin_name)
      if vim.fn.filereadable(setup_file) == 1 then
        if not val.setup then
          val.setup =
            function(name) require(string.format('plugins.setup.%s', require('utils').extract_plugin_name(name))) end
        else
          print(
            string.format(
              'WARNING: plugin %s has both setup file and setup option, the latter will be used',
              plugin_name
            )
          )
        end
      end
      plugins[key][1] = key
      final_table[#final_table + 1] = plugins[key]
    end
  end
  return final_table
end

local packer = require('packer')
packer.init({
  config = {
    compile_path = vim.fn.stdpath('state') .. '/packer_compiled.lua',
  },
})
packer.startup({ process_plugins(plugins) })
