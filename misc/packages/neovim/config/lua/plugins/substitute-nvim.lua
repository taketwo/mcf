return {
  {
    'gbprod/substitute.nvim',
    keys = {
      { 's', function() require('substitute').operator() end, desc = 'Substitute with motion' },
      { 'ss', function() require('substitute').line() end, desc = 'Substitute line' },
      { 'S', function() require('substitute').eol() end, desc = 'Substitute to end of line' },
      { 'kx', function() require('substitute.exchange').operator() end, desc = 'Exchange with motion' },
      { 'kxx', function() require('substitute.exchange').line() end, desc = 'Exchange line' },
      { 's', function() require('substitute').visual() end, mode = 'x', desc = 'Substitute visual selection' },
      { 'X', function() require('substitute.exchange').visual() end, mode = 'x', desc = 'Exchange visual selection' },
    },
  },
}
