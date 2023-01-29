local wk = require('which-key')
wk.register({
  s = { require('substitute').operator, 'Substitute with motion' },
  ss = { require('substitute').line, 'Substitute line' },
  S = { require('substitute').eol, 'Substitute to end of line' },
  kx = { require('substitute.exchange').operator, 'Exchange with motion' },
  kxx = { require('substitute.exchange').line, 'Exchange line' },
})
wk.register({
  s = { require('substitute').visual, 'Substitute visual selection' },
  X = { require('substitute.exchange').visual, 'Exchange visual selection' },
}, { mode = 'x' })
