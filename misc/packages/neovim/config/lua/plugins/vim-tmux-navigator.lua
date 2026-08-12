return {
  {
    'christoomey/vim-tmux-navigator',
    keys = {
      { '<M-h>', '<cmd>TmuxNavigateLeft<cr>', desc = 'Go to left window or pane', mode = { 'n', 't' } },
      { '<M-n>', '<cmd>TmuxNavigateRight<cr>', desc = 'Go to right window or pane', mode = { 'n', 't' } },
      { '<M-c>', '<cmd>TmuxNavigateUp<cr>', desc = 'Go to upper window or pane', mode = { 'n', 't' } },
      { '<M-t>', '<cmd>TmuxNavigateDown<cr>', desc = 'Go to lower window or pane', mode = { 'n', 't' } },
    },
    init = function()
      vim.g.tmux_navigator_no_mappings = 1
      vim.g.tmux_navigator_disable_when_zoomed = 1
    end,
    config = function()
      -- vim-tmux-navigator's TmuxNavigate* commands only know how to hand off
      -- to tmux at a window edge, gated on $TMUX being set; under herdr
      -- instead of tmux they silently no-op there. Redefine them to fall
      -- through to herdr's pane focus in that case, so the same keys above
      -- work under either multiplexer. Leave tmux (and the plain-Neovim
      -- fallback when neither is active) untouched.
      if vim.env.TMUX or not vim.env.HERDR_ENV then
        return
      end
      local function navigate(wincmd_dir, herdr_dir)
        local winnr_before = vim.fn.winnr()
        vim.cmd('wincmd ' .. wincmd_dir)
        if vim.fn.winnr() == winnr_before then
          vim.fn.system({ 'herdr', 'pane', 'focus', '--direction', herdr_dir })
        end
      end
      vim.api.nvim_create_user_command('TmuxNavigateLeft', function() navigate('h', 'left') end, {})
      vim.api.nvim_create_user_command('TmuxNavigateDown', function() navigate('j', 'down') end, {})
      vim.api.nvim_create_user_command('TmuxNavigateUp', function() navigate('k', 'up') end, {})
      vim.api.nvim_create_user_command('TmuxNavigateRight', function() navigate('l', 'right') end, {})
    end,
  },
}
