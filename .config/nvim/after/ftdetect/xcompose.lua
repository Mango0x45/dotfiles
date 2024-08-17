vim.api.nvim_create_autocmd({'BufRead', 'BufNewFile'}, {
	pattern = 'xcompose',
	callback = function()
		vim.bo.filetype = 'xcompose'
	end
})
