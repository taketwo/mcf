return {
  {
    'dyng/ctrlsf.vim',
    keys = {
      { '<C-f>', '<Plug>CtrlSFPrompt' },
      { '<C-f>', '<Plug>CtrlSFVwordExec', mode = 'x' },
    },
    init = function()
      vim.g.ctrlsf_auto_focus = {
        at = 'start',
      }
      vim.g.ctrlsf_mapping = {
        open = 'O',
        openb = { key = 'o', suffix = '<C-w>p' },
        split = '',
        vsplit = '',
        tab = '',
        tabb = '',
        popen = 'p',
        popenf = 'P',
        quit = 'q',
        next = '>',
        prev = '<',
        pquit = '',
        loclist = '',
        stop = '',
      }
    end,
  },
}
