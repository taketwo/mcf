local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

require('packer').startup({
  function(use)
    use 'wbthomason/packer.nvim'

    use { "norcalli/nvim-colorizer.lua",
      opt = true,
      cmd = "ColorizerToggle",
      config = function() require("colorizer").setup() end
    }

    use 'andymass/vim-matchup'
    use 'godlygeek/tabular'
    use 'AndrewRadev/splitjoin.vim'
    use 'lifepillar/vim-solarized8'
    use {
      'vim-airline/vim-airline',
      requires = {'vim-airline/vim-airline-themes'}
    }
    use 'MarcWeber/vim-addon-mw-utils'
    use 'tomtom/tlib_vim'
    use 'kshenoy/vim-signature'
    use 'briandoll/change-inside-surroundings.vim'
    use 'tmsvg/pear-tree'
    use 'scrooloose/nerdcommenter'
    use 'w0rp/ale'
    use 'SirVer/ultisnips'
    use 'tpope/vim-surround'
    use 'embear/vim-localvimrc'
    use 'qpkorr/vim-bufkill'
    use 'AndrewRadev/sideways.vim'
    use 'dantler/vim-alternate'
    use 'tpope/vim-eunuch'
    use 'tpope/vim-dispatch'
    use 'Peeja/vim-cdo'
    use 'tpope/vim-abolish'
    use 'mbbill/undotree'
    use 'xolox/vim-misc'
    use 'taketwo/vim-exchange'
    use 'janko-m/vim-test'
    use 'AndrewRadev/dsf.vim'
    use 'tweekmonster/startuptime.vim'
    use 'sickill/vim-pasta'
    use 'taketwo/vimux'
    use 'jeffkreeftmeijer/vim-numbertoggle'
    use 'taketwo/diffchar.vim'
    use 'AndrewRadev/deleft.vim'

    -- Navigation
    use 'justinmk/vim-sneak'
    use 'christoomey/vim-tmux-navigator'
    use 'pechorin/any-jump.vim'

    -- Search
    use 'henrik/vim-indexed-search'
    use 'nelstrom/vim-visual-star-search'
    use 'dyng/ctrlsf.vim'

    use 'preservim/vim-markdown'
    use {
      'taketwo/vim-ros',
      -- branch = 'cmp'
    }
    use 'tikhomirov/vim-glsl'
    use 'digitaltoad/vim-pug'
    use 'lervag/vimtex'
    use 'keith/tmux.vim'
    use 'LnL7/vim-nix'
    use 'hjson/vim-hjson'
    use { 'jkramer/vim-checkbox',
      opt = true,
      ft = { 'markdown' }
    }

    -- Git related
    use 'tpope/vim-fugitive'
    use 'tpope/vim-git'
    use 'tpope/vim-rhubarb'
    use 'airblade/vim-gitgutter'
    use 'junegunn/gv.vim'
    use 'whiteinge/diffconflicts'
    use 'rhysd/git-messenger.vim'
    use 'jreybert/vimagit'

    -- Text objects
    use 'michaeljsmith/vim-indent-object'
    use 'coderifous/textobj-word-column.vim'
    use 'kana/vim-textobj-entire'
    use 'kana/vim-textobj-user'
    use 'kana/vim-textobj-function'
    use 'jceb/vim-textobj-uri'
    use 'machakann/vim-textobj-delimited'

    -- Snippets
    use 'honza/vim-snippets'
    use 'kkoomen/vim-doge'

    -- Misc
    use 'liuchengxu/vim-which-key'
    use 'voldikss/vim-floaterm'

    -- Misc unsorted
    use 'kevinhwang91/rnvimr'
    use {
      'nvim-treesitter/nvim-treesitter',
      run = ':TSUpdate',
      config = function() require 'plugins.configs.treesitter' end
    }
    use {
      'nvim-treesitter/playground',
      -- opt = true,
      -- cmd = {'TSHighlightCapturesUnderCursor'}
    }
    use {
      'nvim-telescope/telescope.nvim',
      requires = {{'nvim-lua/popup.nvim'}, {"nvim-lua/plenary.nvim"}},
      config = function() require 'plugins.configs.telescope' end
    }
    use {'nvim-telescope/telescope-fzf-native.nvim', run = 'make' }
    use 'nvim-telescope/telescope-symbols.nvim'
    use 'nvim-telescope/telescope-ui-select.nvim'
    use 'nvim-telescope/telescope-file-browser.nvim'
    use {
      'neovim/nvim-lspconfig',
      config = function() require 'plugins.configs.lsp' end
    }
    use {
      'glepnir/lspsaga.nvim',
      config = function() require('lspsaga').init_lsp_saga() end
    }
    use {
      'tversteeg/registers.nvim',
      config = function() require('registers').setup() end
    }
    use {
      'lukas-reineke/indent-blankline.nvim',
      config = function() require 'plugins.configs.indent_blankline' end
    }
    use 'onsails/lspkind-nvim'
    use {
      'folke/todo-comments.nvim',
      config = function() require 'plugins.configs.todo_comments' end
    }
    use { 'nvim-tree/nvim-tree.lua',
      requires = { 'nvim-tree/nvim-web-devicons' },
      opt = true,
      cmd = { 'NvimTreeToggle', 'NvimTreeOpen' },
      config = function() require('nvim-tree').setup() end
    }

    -- Autocompletion
    use { 'hrsh7th/nvim-cmp',
      config = function() require 'plugins.configs.cmp' end
    }
    use 'hrsh7th/cmp-nvim-lsp'
    use 'hrsh7th/cmp-buffer'
    use 'hrsh7th/cmp-omni'
    use 'hrsh7th/cmp-path'
    use 'quangnguyen30192/cmp-nvim-ultisnips'
    use { 'zbirenbaum/copilot.lua',
      event = 'VimEnter',
      config = function()
        vim.defer_fn(function()
          require('copilot').setup({
            suggestion = {
              enabled = true,
              auto_trigger = true,
              debounce = 75,
              keymap = {
               accept = "<Tab>",
               next = "<M-]>",
               prev = "<M-[>",
               dismiss = "<C-]>",
              },
            },
            copilot_node_command = vim.fn.expand("$FNM_DIR") .. "/aliases/node17/bin/node"
          })
        end, 100)
      end,
    }

    -- Automatically set up your configuration after cloning packer.nvim
    -- Put this at the end after all plugins
    if packer_bootstrap then
      require('packer').sync()
    end
  end,
  config = {
    compile_path = vim.fn.stdpath('state') .. '/packer_compiled.lua',
  }
})
