nnoremap € $
vnoremap € $
nnoremap ) 0
vnoremap ) 0
nnoremap < <<
nnoremap > >>
nnoremap K :vert Man<cr>
vnoremap K :vert Man<cr>
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l
nnoremap V <c-v>
nnoremap <c-v> V
nnoremap <c-n> :make<cr>

fu! SortWords()
	if visualmode() == "V"
		'<,'>!sort -
	else
		'<,'>s/\%V.*\%V\w*/\=join(sort(split(submatch(0))), ' ')
	endif
endfu

vnoremap s :<c-u>call SortWords()<cr>

set noet
set tw=80 colorcolumn=81
set secure exrc
set splitright splitbelow
