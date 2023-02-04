return {
  {
    'jceb/vim-textobj-uri',
    dependencies = { 'kana/vim-textobj-user' },
    keys = {
      { 'iu', mode = { 'o', 'x' }, desc = 'uri' },
      { 'au', mode = { 'o', 'x' }, desc = 'uri (with white space)' },
    },
  },
}
