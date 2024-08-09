vim.api.nvim_create_autocmd({'BufRead', 'BufNewFile'}, {
	pattern = '*.yx',
	callback = function()
		vim.bo.filetype = 'oryx'
	end,
})
