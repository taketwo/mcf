" ALE config
let b:ale_enabled = 1
" Linters
let b:ale_linters = []
let g:ale_cpp_cppcheck_options = '--enable=all --project=build/compile_commands.json --inline-suppr'
" Fixers
let b:ale_fixers = [ 'shfmt' ]
let g:ale_sh_shfmt_options = '-i 2 -ci'
