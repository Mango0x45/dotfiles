local confhome = vim.fn.getenv('XDG_CONFIG_HOME')
confhome = confhome == vim.NIL and '~/.config/aerc/' or confhome .. '/aerc/'

vim.api.nvim_create_autocmd({'BufRead', 'BufNewFile'}, {
	pattern = {
		confhome .. '*.conf',
		confhome .. 'stylesets/*',
		'/usr/share/aerc/*.conf',
		'/usr/share/aerc/stylesets/*',
	},
	callback = function()
		vim.bo.filetype = 'ini'
	end,
})

vim.api.nvim_create_autocmd({'BufRead', 'BufNewFile'}, {
	pattern = {
		confhome .. 'templates/*',
		'/usr/share/aerc/templates/*',
	},
	callback = function()
		vim.bo.filetype = 'gotmpl'
	end,
})
