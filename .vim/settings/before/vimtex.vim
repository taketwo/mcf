let g:tex_flavor = 'latex'

let g:vimtex_compiler_enabled = 0
let g:vimtex_mappings_enabled = 0

if !exists('g:ycm_semantic_triggers')
    let g:ycm_semantic_triggers = {}
endif

let g:ycm_semantic_triggers.tex = [
\   're!\\[A-Za-z]*cite[A-Za-z]*(\[[^]]*\]){0,2}{[^}]*',
\   're!\\[A-Za-z]*ref({[^}]*|range{([^,{}]*(}{)?))',
\   're!\\hyperref\[[^]]*',
\   're!\\includegraphics\*?(\[[^]]*\]){0,2}{[^}]*',
\   're!\\(include(only)?|input){[^}]*',
\   're!\\\a*(gls|Gls|GLS)(pl)?\a*(\s*\[[^]]*\]){0,2}\s*\{[^}]*',
\   're!\\includepdf(\s*\[[^]]*\])?\s*\{[^}]*',
\   're!\\includestandalone(\s*\[[^]]*\])?\s*\{[^}]*',
\   're!\\usepackage(\s*\[[^]]*\])?\s*\{[^}]*',
\   're!\\documentclass(\s*\[[^]]*\])?\s*\{[^}]*',
\ ]
