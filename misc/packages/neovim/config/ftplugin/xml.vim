" Disable LSP autoformatting for the time being
let b:autoformat = v:false

" Setup auto-pairs (based on HTML)
let b:pear_tree_pairs = extend(deepcopy(g:pear_tree_pairs), {
            \ '<*>': {'closer': '</*>',
            \         'not_if': [],
            \         'not_like': '/$',
            \         'until': '[^a-zA-Z0-9-._]'
            \        },
            \ '<!--': {'closer': '-->'},
            \ }, 'keep')
