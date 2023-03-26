" ALE
" Disable shellcheck and language_server linters because Bash language server includes them.
let b:ale_linters_ignore = ['shellcheck', 'language_server']
