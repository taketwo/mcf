require('which-key').register({
  ['<Leader>g'] = {
    u = { 'Copy file URL' },
    U = { '<cmd>lua require("gitlinker").get_repo_url()<cr>', 'Copy repository URL' },
  },
  ['<Leader>gu'] = { 'Copy line(s) URL', mode = 'v' }
})
