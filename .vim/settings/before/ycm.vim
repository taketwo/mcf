" Do not use S-Tab
let g:ycm_key_list_previous_completion = ['<Up>']
" Do not use <Leader>d
let g:ycm_key_detailed_diagnostics = '<Leader>D'
" Use same error/warning symbols as ALE
let g:ycm_error_symbol = g:ale_sign_error
let g:ycm_warning_symbol = g:ale_sign_warning

" Populate location list with warnings such that >/< mapping can be used to
" jump between them
let g:ycm_always_populate_location_list = 1

let g:ycm_global_ycm_extra_conf = $MCF.'/misc/.ycm_extra_conf.py'

let g:ycm_add_preview_to_completeopt = 1
let g:ycm_autoclose_preview_window_after_completion = 0
let g:ycm_autoclose_preview_window_after_insertion = 1

function! BuildYCM(info)
    if a:info.status == 'installed' || a:info.status == 'updated' || a:info.force
        let opts = ' --clang-completer'
        if tlib#sys#IsExecutable('npm')
            let opts = opts.' --js-completer'
        endif
        if has('nvim')
            execute '!'.g:python3_host_prog.' install.py'.opts
        elseif has('python3')
            execute '!python3 install.py'.opts
        else
            execute '!python install.py'.opts
        endif
    endif
endfunction

" The portion of the config below is inspired by:
"    https://www.linkedin.com/pulse/can-vim-detect-pipenv-environment-vagiz-duseev
" Its purpose is to setup YCM's python in accordance to the current pipenv/poetry.

" At first, check for a local virtualenv directory with python inside.
" FIXME: this does not work if vim is started not from the project root.
if filereadable('.venv/bin/python')
  " We are done!
  let g:ycm_python_binary_path = '.venv/bin/python'
else
  " Investigate a possibility that there is a virtualenv (somewhere) managed by pipenv.
  " At first, get the output of 'pipenv --venv' command.
  let pipenv_venv_path = system('pipenv --venv')
  " The above system() call produces a non zero exit code whenever
  " a proper virtual environment has not been found.
  " So, second, we only point YCM to the virtual environment when
  " the call to 'pipenv --venv' was successful.
  " Remember, that 'pipenv --venv' only points to the root directory
  " of the virtual environment, so we have to append a full path to
  " the python executable.
  if v:shell_error == 0
    let venv_path = substitute(pipenv_venv_path, '\n', '', '')
    let g:ycm_python_binary_path = venv_path . '/bin/python'
  else
    if has('nvim')
        let g:ycm_python_binary_path = g:python3_host_prog
    else
        let g:ycm_python_binary_path = 'python'
    endif
  endif
endif
