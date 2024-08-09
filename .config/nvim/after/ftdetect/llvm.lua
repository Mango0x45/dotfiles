vim.api.nvim_create_autocmd({'BufRead', 'BufNewFile'}, {
	pattern = '*.ll',
	callback = function()
		vim.bo.filetype = 'llvm'
	end,
})
