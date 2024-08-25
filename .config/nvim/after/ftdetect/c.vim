" Vim by default sets *.h files to be cpp, so we need to explicitly override that
autocmd BufRead,BufNewFile *.h set filetype=c
