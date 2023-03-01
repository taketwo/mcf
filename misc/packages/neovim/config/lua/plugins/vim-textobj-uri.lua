return {
  {
    'jceb/vim-textobj-uri',
    dependencies = { 'kana/vim-textobj-user' },
    keys = {
      { 'iu', mode = { 'o', 'x' }, desc = 'Uri' },
      { 'au', mode = { 'o', 'x' }, desc = 'Uri including white-space' },
    },
  },
}
