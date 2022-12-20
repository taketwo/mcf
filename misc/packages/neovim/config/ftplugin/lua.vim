" ALE config
" Fixers
let b:ale_fixers = ['stylua']
let g:ale_lua_stylua_options = '--config-path ' . stdpath('config') . '/extras/stylua.toml'
