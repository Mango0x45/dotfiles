function! s:gitignore_files()
	silent! !git rev-parse --is-inside-work-tree
	if v:shell_error == 0
		return netrw_gitignore#Hide()
	endif
	return '^$' " Shouldn’t match anything
endfunction

let g:netrw_banner = 0
let g:netrw_bufsettings = 'noma nomod nu nobl nowrap ro' " Enables line-numbers
let g:netrw_list_hide = s:gitignore_files()
			\ .. ',^\(\.\|\.\.\)/\?$'
			\ .. ',.*\.o$'
