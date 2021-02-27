if has("nvim")

    " Enable auto popup, alternatively Tab can be setup as a trigger key
    let g:completion_enable_auto_popup = 1

    " Enable snippets support
    let g:completion_enable_snippet = 'UltiSnips'

    " Smart case matching
    let g:completion_matching_smart_case = 1

    " Trigger completion after 2 characters
    let g:completion_trigger_keyword_length = 2

    " Other possibilities:

    " Eventually, we may enable completion-nvim in every buffer
    " autocmd BufEnter * lua require'completion'.on_attach()

    " Setup additional sources (e.g. buffers, treesitter, tags)

    " Setup chain completion https://github.com/nvim-lua/completion-nvim/wiki/chain-complete-support
    " Customize per filetype or even syntax scope

endif
