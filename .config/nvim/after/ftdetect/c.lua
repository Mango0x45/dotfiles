vim.api.nvim_create_autocmd({'BufRead', 'BufNewFile'}, {
	pattern = '*.[ch]',
	callback = function()
		vim.bo.filetype = 'c'
	end,
})
