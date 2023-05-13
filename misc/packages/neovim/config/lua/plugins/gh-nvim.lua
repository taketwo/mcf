return {
  {
    'ldelossa/gh.nvim',
    dependencies = {
      { 'ldelossa/litee.nvim' },
      {
        'folke/which-key.nvim',
        opts = {
          defaults = {
            ['<Leader>g'] = {
              name = 'Git',
              g = {
                name = 'Github',
                c = { name = 'Commits' },
                i = { name = 'Issues' },
                l = { name = 'Litee' },
                r = { name = 'Review' },
                p = { name = 'Pull Request' },
                t = { name = 'Threads' },
              },
            },
          },
        },
      },
    },
    keys = {
      { '<Leader>ggcc', '<cmd>GHCloseCommit<cr>', desc = 'Close' },
      { '<Leader>ggce', '<cmd>GHExpandCommit<cr>', desc = 'Expand' },
      { '<Leader>ggco', '<cmd>GHOpenToCommit<cr>', desc = 'Open to' },
      { '<Leader>ggcp', '<cmd>GHPopOutCommit<cr>', desc = 'Pop out' },
      { '<Leader>ggcz', '<cmd>GHCollapseCommit<cr>', desc = 'Collapse' },
      { '<Leader>ggip', '<cmd>GHPreviewIssue<cr>', desc = 'Preview' },
      { '<Leader>gglt', '<cmd>LTPanel<cr>', desc = 'Toggle panel' },
      { '<Leader>ggpc', '<cmd>GHClosePR<cr>', desc = 'Close' },
      { '<Leader>ggpd', '<cmd>GHPRDetails<cr>', desc = 'Details' },
      { '<Leader>ggpe', '<cmd>GHExpandPR<cr>', desc = 'Expand' },
      { '<Leader>ggpo', '<cmd>GHOpenPR<cr>', desc = 'Open' },
      { '<Leader>ggpp', '<cmd>GHPopOutPR<cr>', desc = 'Pop out' },
      { '<Leader>ggpr', '<cmd>GHRefreshPR<cr>', desc = 'Refresh' },
      { '<Leader>ggpt', '<cmd>GHOpenToPR<cr>', desc = 'Open to' },
      { '<Leader>ggpz', '<cmd>GHCollapsePR<cr>', desc = 'Collapse' },
      { '<Leader>ggrb', '<cmd>GHStartReview<cr>', desc = 'Begin' },
      { '<Leader>ggrc', '<cmd>GHCloseReview<cr>', desc = 'Close' },
      { '<Leader>ggrd', '<cmd>GHDeleteReview<cr>', desc = 'Delete' },
      { '<Leader>ggre', '<cmd>GHExpandReview<cr>', desc = 'Expand' },
      { '<Leader>ggrs', '<cmd>GHSubmitReview<cr>', desc = 'Submit' },
      { '<Leader>ggrz', '<cmd>GHCollapseReview<cr>', desc = 'Collapse' },
      { '<Leader>ggtc', '<cmd>GHCreateThread<cr>', desc = 'Create' },
      { '<Leader>ggtn', '<cmd>GHNextThread<cr>', desc = 'Next' },
      { '<Leader>ggtt', '<cmd>GHToggleThread<cr>', desc = 'Toggle' },
    },
    config = function(_, opts)
      require('litee.lib').setup()
      require('litee.gh').setup(opts)
    end,
  },
}
