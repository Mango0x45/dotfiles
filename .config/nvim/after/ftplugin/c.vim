setlocal commentstring=//\ %s

function! s:ManpageAtPoint()
	let l:w = expand('<cword>')
	call system(['env', 'MANSECT=3,2,3p', 'man', l:w])
	if v:shell_error == 0
		execute "silent !MANSECT=3,2,3p man -Tpdf '" . l:w . "' | zathura - &"
	elseif v:shell_error == 16
		echomsg 'No manual for ‘' . l:w . '’ found'
	else
		echomsg 'An error occured running ‘man’'
	endif
endfunction

xnoremap <buffer> <silent> <LocalLeader>q <Cmd>!clang-format -style=file<CR>
nnoremap <buffer> <silent> K :call <SID>ManpageAtPoint()<CR>
