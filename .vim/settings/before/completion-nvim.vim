if has("nvim")

    " Enable auto popup
    " In addition to that, Tab will be setup as a trigger key in keymaps.vim
    let g:completion_enable_auto_popup = 1

    " Enable snippets support
    let g:completion_enable_snippet = 'UltiSnips'

    " Matching with smart case and fuzziness
    let g:completion_matching_smart_case = 1
    let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy', 'all']

    " Trigger completion after 2 characters
    let g:completion_trigger_keyword_length = 2

    " Limit the width of the popup menu
    let g:completion_menu_length = 50
    let g:completion_abbr_length = 50

    " Other possibilities:

    " Eventually, we may enable completion-nvim in every buffer
    " autocmd BufEnter * lua require'completion'.on_attach()

    " Setup additional sources (e.g. buffers, treesitter, tags)

    " Setup chain completion https://github.com/nvim-lua/completion-nvim/wiki/chain-complete-support
    " Customize per filetype or even syntax scope

endif
