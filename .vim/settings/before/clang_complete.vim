let g:clang_use_library = 1
let g:clang_library_path = "/opt/llvm/lib"
" Disable auto popup, use <Tab> to autocomplete
let g:clang_complete_auto = 0
" Show clang errors in the quickfix window
let g:clang_complete_copen = 1
" Automatically select the first entry
let g:clang_auto_select = 1
" Use ultisnips engine for auto-completion
let g:clang_snippets = 1
let g:clang_snippets_engine = "ultisnips"
" Jump to the end after entering all parameters
let g:clang_trailing_placeholder = 1
" Avoid remapping <C-O> to <C-T> which will happen by default
let g:clang_jumpto_back_key = "<C-O>"
" Also look for compilation options in CMake compilation database
let g:clang_auto_user_options = "path, .clang_complete, compile_commands.json"
let g:clang_compilation_database = "build"
