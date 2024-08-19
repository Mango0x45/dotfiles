if &cp || exists('g:loaded_scratch')
	finish
endif
let g:loaded_scratch = v:true

function! scratch#New(cmd)
	execute a:cmd
	if bufexists('scratch')
		buffer scratch
	else
		noswapfile hide enew
		setlocal buftype=nofile bufhidden=hide
		file scratch
	endif
endfunction

command! -nargs=0  Scratch call scratch#New('split')
command! -nargs=0 VScratch call scratch#New('vsplit')
