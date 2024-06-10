vim.bo.commentstring = '/* %s */'

vim.keymap.set('v', '<localleader>=', ":'<'>!clang-format -style=file -<CR>", {
	desc = 'Format the current selection with Clang Format',
})

vim.keymap.set('n', 'K', function()
	local w = vim.fn.expand('<cword>')
	local proc = vim.system({'man', w}):wait()
	if proc.code == 16 then
		print('No manual for ‘' .. w .. '’ found')
		return
	elseif proc.code ~= 0 then
		print('An error occured')
		return
	end
	vim.cmd [[
		execute "silent !man -Tpdf '" . expand('<cword>') . "' | zathura - &"
	]]
end, {
	buffer = 0,
	desc = 'View the manual page for the word under the cursor',
})
