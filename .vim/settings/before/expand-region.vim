if !has('nvim')
    nmap = <Plug>(expand_region_expand)
    vmap = <Plug>(expand_region_expand)
    nmap + <Plug>(expand_region_shrink)
    vmap + <Plug>(expand_region_shrink)
endif
