vim.bo.commentstring = '/* %s */'

vim.keymap.set('v', '<localleader>=', ":'<'>!clang-format -style=file -<CR>", {
	desc = 'Format the current selection with Clang Format',
})

vim.keymap.set('n', 'K', function()
	vim.cmd [[
		execute "silent !man -Tpdf '" . expand('<cword>') . "' | zathura - &"
	]]
end, {
	buffer = 0,
	desc = 'View the manual page for the word under the cursor',
})
