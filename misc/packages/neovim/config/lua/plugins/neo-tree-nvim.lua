return {
  {
    'nvim-neo-tree/neo-tree.nvim',
    dependencies = {
      { 'MunifTanjim/nui.nvim' },
      { 'echasnovski/mini.icons' },
      { 'nvim-lua/plenary.nvim' },
    },
    cmd = 'Neotree',
    keys = {
      {
        '<F12>',
        function() require('neo-tree.command').execute({ toggle = true, dir = LazyVim.root() }) end,
        desc = 'Toggle NeoTree',
      },
      {
        '<Leader>ge',
        function() require('neo-tree.command').execute({ source = 'git_status', toggle = true }) end,
        desc = 'Explorer',
      },
      {
        '<Leader>be',
        function() require('neo-tree.command').execute({ source = 'buffers', toggle = true }) end,
        desc = 'Explorer',
      },
    },
    deactivate = function() vim.cmd([[Neotree close]]) end,
    init = function()
      -- FIX: use `autocmd` for lazy-loading neo-tree instead of directly requiring it,
      -- because `cwd` is not set up properly.
      vim.api.nvim_create_autocmd('BufEnter', {
        group = vim.api.nvim_create_augroup('Neotree_start_directory', { clear = true }),
        desc = 'Start Neo-tree with directory',
        once = true,
        callback = function()
          if package.loaded['neo-tree'] then
            return
          else
            local stats = vim.uv.fs_stat(vim.fn.argv(0))
            if stats and stats.type == 'directory' then require('neo-tree') end
          end
        end,
      })
    end,
    opts = {
      sources = { 'filesystem', 'buffers', 'git_status', 'document_symbols' },
      open_files_do_not_replace_types = { 'terminal', 'Trouble', 'qf', 'Outline', 'edgy' },
      filesystem = {
        bind_to_cwd = false,
        follow_current_file = { enabled = true },
        use_libuv_file_watcher = true,
        filtered_items = { visible = true },
      },
      window = {
        mappings = {
          ['<space>'] = 'none',
          ['c'] = 'none', -- do not map anything such that it functions as up
          ['t'] = 'none', -- do not map anything such that it functions as down
          ['h'] = 'close_node',
          ['n'] = 'open',
          ['Y'] = {
            function(state)
              local node = state.tree:get_node()
              local path = node:get_id()
              vim.fn.setreg('+', path, 'c')
            end,
            desc = 'copy_path_to_clipboard',
          },
        },
      },
      default_component_configs = {
        indent = {
          with_expanders = true,
        },
      },
    },
    config = function(_, opts)
      local function on_move(data) Snacks.rename.on_rename_file(data.source, data.destination) end
      local events = require('neo-tree.events')
      opts.event_handlers = opts.event_handlers or {}
      vim.list_extend(opts.event_handlers, {
        { event = events.FILE_MOVED, handler = on_move },
        { event = events.FILE_RENAMED, handler = on_move },
      })
      require('neo-tree').setup(opts)
    end,
  },
}
