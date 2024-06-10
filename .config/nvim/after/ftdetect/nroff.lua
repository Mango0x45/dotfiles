vim.api.nvim_create_autocmd({'BufRead', 'BufNewFile'}, {
	pattern = '*.[1-7]',
	callback = function()
		vim.bo.filetype = 'nroff'
	end,
})
