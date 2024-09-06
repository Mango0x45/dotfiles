if &cp || exists('g:loaded_zwspace')
	finish
endif
let g:loaded_zwspace = v:true

function s:ConcealZWSpaces()
	if exists('w:zwspaces')
		for i in w:zwspaces
			call matchdelete(i)
		endfor
		unlet w:zwspaces
	endif
	let w:zwspaces = [
		\	matchadd('Conceal', "\u200B"),
		\	matchadd('Conceal', "\u200C"),
		\	matchadd('Conceal', "\u200D"),
		\	matchadd('Conceal', "\uFEFF"),
		\ ]
endfunction

autocmd BufEnter * call <SID>ConcealZWSpaces()
