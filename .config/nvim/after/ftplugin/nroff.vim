setlocal textwidth=73

nnoremap <LocalLeader>c :w !wc -w<CR>
xnoremap <LocalLeader>c :'<,'>w !deroff \| wc -w<CR>
