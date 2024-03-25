local Util = require('mcf.util')

return {
  {
    'nvim-neo-tree/neo-tree.nvim',
    branch = 'v3.x',
    dependencies = {
      'MunifTanjim/nui.nvim',
      'nvim-lua/plenary.nvim',
      'nvim-tree/nvim-web-devicons',
    },
    cmd = 'Neotree',
    keys = {
      {
        '<F12>',
        function() require('neo-tree.command').execute({ toggle = true, dir = Util.root() }) end,
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
      if vim.fn.argc(-1) == 1 then
        local stat = vim.uv.fs_stat(vim.fn.argv(0))
        if stat and stat.type == 'directory' then require('neo-tree') end
      end
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
      local function on_move(data) Util.lsp.on_rename(data.source, data.destination) end
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
