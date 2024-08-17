if &cp || exists('g:loaded_winmove')
	finish
endif
let g:loaded_winmove = v:true

function! winmove#Next()
	let l:n1 = winnr()
	wincmd l
	if l:n1 != winnr()
		return
	endif

	tabnext

	let l:n1 = winnr()
	while v:true
		wincmd h
		let l:n2 = winnr()
		if l:n1 == l:n2
			return
		endif
		let l:n1 = l:n2
	endwhile
endfunction

function! winmove#Prev()
	let l:n1 = winnr()
	wincmd h
	if l:n1 != winnr()
		return
	endif

	tabprev

	let l:n1 = winnr()
	while v:true
		wincmd l
		let l:n2 = winnr()
		if l:n1 == l:n2
			return
		endif
		let l:n1 = l:n2
	endwhile
endfunction

command! -nargs=0 WinMoveNext call winmove#Next()
command! -nargs=0 WinMovePrev call winmove#Prev()
