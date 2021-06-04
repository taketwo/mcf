if has("nvim")

    " Enable auto popup
    " In addition to that, Tab will be setup as a trigger key in keymaps.vim
    let g:completion_enable_auto_popup = 1

    " Enable snippets support
    let g:completion_enable_snippet = 'UltiSnips'

    " Matching with smart case and fuzziness
    let g:completion_matching_smart_case = 1
    let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy', 'all']

    " Trigger completion after 1 character
    let g:completion_trigger_keyword_length = 1

    " Limit the width of the popup menu
    let g:completion_menu_length = 50
    let g:completion_abbr_length = 50

    " Enable completion-nvim in every buffer
    autocmd BufEnter * lua require('completion').on_attach()

    " Automatically skip empty sources and add keymap to cycle through them
    let g:completion_auto_change_source = 1
    imap <c-j> <Plug>(completion_next_source)

    let g:completion_chain_complete_list = {
    \ 'cpp': {
    \   'default': [
    \     {'complete_items': ['lsp', 'snippet']},
    \     {'mode': 'omni'},
    \   ],
    \   'comment': []
    \ },
    \ 'gtest.cpp': {
    \   'default': [
    \     {'complete_items': ['lsp', 'snippet']},
    \     {'mode': 'omni'},
    \   ],
    \   'comment': []
    \ },
    \ 'sh': [
    \   {'complete_items': ['lsp', 'snippet']}
    \ ],
    \ 'rosmsg' : [ {'mode': 'omni'} ],
    \ 'rossrv' : [ {'mode': 'omni'} ],
    \ 'rosaction' : [ {'mode': 'omni'} ],
    \ 'roslaunch.xml' : [ {'mode': 'omni'}, {'complete_items': ['snippet'] } ],
    \ 'magit' : [ {'mode': '<c-n>'}, {'mode': '<c-p>'} ],
    \ 'TelescopePrompt' : [ ],
    \ 'default' : [
    \    {'complete_items': ['lsp', 'snippet'] },
    \    {'complete_items': ['path'], 'triggered_only': ['/'] },
    \    {'mode': '<c-p>'},
    \    {'mode': '<c-n>'},
    \ ],
    \}

    " Other possibilities:

    " Setup additional sources (e.g. buffers, treesitter, tags)

    " Setup chain completion https://github.com/nvim-lua/completion-nvim/wiki/chain-complete-support
    " Customize per filetype or even syntax scope
endif
