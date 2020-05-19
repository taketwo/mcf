if exists("g:Abolish")
  " Remove cr mapping
  nunmap cr
  " Use kr instead
  nmap kr <Plug>Coerce
endif
