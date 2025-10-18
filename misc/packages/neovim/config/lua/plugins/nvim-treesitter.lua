return {
  {
    'nvim-treesitter/nvim-treesitter',
    dependencies = {
      {
        'nvim-treesitter/nvim-treesitter-textobjects',
        branch = 'main',
        event = 'VeryLazy',
        opts = {},
        config = function(_, opts)
          local TS = require('nvim-treesitter-textobjects')
          if not TS.setup then
            LazyVim.error('Please use `:Lazy` and update `nvim-treesitter`')
            return
          end
          TS.setup(opts)

          local function attach(buf)
            local ft = vim.bo[buf].filetype
            if not LazyVim.treesitter.have(ft, 'textobjects') then return end

            local moves = {
              goto_next_start = { [']f'] = '@function.outer', [']c'] = '@class.outer', [']p'] = '@parameter.inner' },
              goto_next_end = { [']F'] = '@function.outer', [']C'] = '@class.outer', [']P'] = '@parameter.inner' },
              goto_previous_start = {
                ['[f'] = '@function.outer',
                ['[c'] = '@class.outer',
                ['[p'] = '@parameter.inner',
              },
              goto_previous_end = { ['[F'] = '@function.outer', ['[C'] = '@class.outer', ['[P'] = '@parameter.inner' },
            }
            for method, keymaps in pairs(moves) do
              for key, query in pairs(keymaps) do
                local desc = query:gsub('@', ''):gsub('%..*', '')
                desc = (key:sub(1, 1) == '[' and 'Previous ' or 'Next ') .. desc
                desc = desc .. (key:sub(2, 2) == key:sub(2, 2):upper() and ' end' or ' start')
                --Only set the keymap if we are not in diff mode with a c/C key
                if not (vim.wo.diff and key:find('[cC]')) then
                  vim.keymap.set(
                    { 'n', 'x', 'o' },
                    key,
                    function() require('nvim-treesitter-textobjects.move')[method](query, 'textobjects') end,
                    { desc = desc, buffer = buf, silent = true }
                  )
                end
              end
            end
            local swaps = {
              swap_next = {
                ['<Leader>sn'] = { query = '@parameter.inner', desc = 'Shift parameter right' },
                ['<Leader>st'] = { query = '@function.outer', desc = 'Shift function down' },
                ['<Leader>sT'] = { query = '@class.outer', desc = 'Shift class down' },
              },
              swap_previous = {
                ['<Leader>sh'] = { query = '@parameter.inner', desc = 'Shift parameter left' },
                ['<Leader>sc'] = { query = '@function.outer', desc = 'Shift function up' },
                ['<Leader>sC'] = { query = '@class.outer', desc = 'Shift class up' },
              },
            }
            for method, keymaps in pairs(swaps) do
              for key, qd in pairs(keymaps) do
                vim.keymap.set(
                  { 'n', 'x' },
                  key,
                  function() require('nvim-treesitter-textobjects.swap')[method](qd['query']) end,
                  { desc = qd['desc'], buffer = buf, silent = true }
                )
              end
            end
          end

          vim.api.nvim_create_autocmd('FileType', {
            group = vim.api.nvim_create_augroup('lazyvim_treesitter_textobjects', { clear = true }),
            callback = function(ev) attach(ev.buf) end,
          })

          vim.tbl_map(attach, vim.api.nvim_list_bufs())
        end,
      },
      {
        'nvim-treesitter/nvim-treesitter-context',
        opts = function()
          local tsc = require('treesitter-context')
          Snacks.toggle({
            name = 'treesitter context',
            get = tsc.enabled,
            set = function(state)
              if state then
                tsc.enable()
              else
                tsc.disable()
              end
            end,
          }):map('<Leader>ut')
          return { mode = 'cursor', max_lines = 3 }
        end,
      },
    },
    branch = 'main',
    build = function()
      local TS = require('nvim-treesitter')
      if not TS.get_installed then
        LazyVim.error('Please restart Neovim and run `:TSUpdate` to use the `nvim-treesitter` **main** branch.')
        return
      end
      LazyVim.treesitter.build(function() TS.update(nil, { summary = true }) end)
    end,
    event = { 'BufReadPost', 'BufNewFile' },
    cmd = { 'TSUpdate', 'TSUpdateSync', 'TSInstall', 'TSLog', 'TSUninstall' },
    ---@class lazyvim.TSConfig: TSConfig
    opts = {
      -- Some of the following parsers are installed by default, but we want to ensure that our config is
      -- not affected by future changes to the default parsers.
      ensure_installed = {
        'bash',
        'c',
        'cmake',
        'comment',
        'cpp',
        'diff',
        'gitcommit',
        'glsl',
        'haskell',
        'hcl', -- for Terraform files
        -- 'hjson', seems to be broken, using a plugin instead
        'html',
        'json',
        'jsonc',
        'lua',
        'luadoc',
        'luap',
        'make',
        'markdown',
        'markdown_inline',
        'nix',
        'printf',
        'proto',
        'python',
        'regex',
        'robot',
        'terraform',
        'toml',
        'vim',
        'vimdoc',
        'xml',
        'yaml',
      },
      highlight = { enable = true },
      indent = { enable = true },
      folds = { enable = true },
      matchup = {
        enable = true,
        disable_virtual_text = true,
      },
    },
    ---@param opts lazyvim.TSConfig
    config = function(_, opts)
      -- NOTE: This function was copied without changes from LazyVim

      local TS = require('nvim-treesitter')

      -- some quick sanity checks
      if not TS.get_installed then
        return LazyVim.error('Please use `:Lazy` and update `nvim-treesitter`')
      elseif type(opts.ensure_installed) ~= 'table' then
        return LazyVim.error('`nvim-treesitter` opts.ensure_installed must be a table')
      end

      -- setup treesitter
      TS.setup(opts)
      LazyVim.treesitter.get_installed(true) -- initialize the installed langs

      -- install missing parsers
      local install = vim.tbl_filter(
        function(lang) return not LazyVim.treesitter.have(lang) end,
        opts.ensure_installed or {}
      )
      if #install > 0 then
        LazyVim.treesitter.build(function()
          TS.install(install, { summary = true }):await(function()
            LazyVim.treesitter.get_installed(true) -- refresh the installed langs
          end)
        end)
      end

      vim.api.nvim_create_autocmd('FileType', {
        group = vim.api.nvim_create_augroup('lazyvim_treesitter', { clear = true }),
        callback = function(ev)
          if not LazyVim.treesitter.have(ev.match) then return end

          -- highlighting
          if vim.tbl_get(opts, 'highlight', 'enable') ~= false then pcall(vim.treesitter.start) end

          -- indents
          if vim.tbl_get(opts, 'indent', 'enable') ~= false and LazyVim.treesitter.have(ev.match, 'indents') then
            LazyVim.set_default('indentexpr', 'v:lua.LazyVim.treesitter.indentexpr()')
          end

          -- folds
          if vim.tbl_get(opts, 'folds', 'enable') ~= false and LazyVim.treesitter.have(ev.match, 'folds') then
            if LazyVim.set_default('foldmethod', 'expr') then
              LazyVim.set_default('foldexpr', 'v:lua.LazyVim.treesitter.foldexpr()')
            end
          end
        end,
      })
    end,
  },
}
