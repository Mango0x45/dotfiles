vim.api.nvim_create_autocmd('BufWinEnter', {
	desc = 'Open (neo)vim help pages in a vertical split',
	group = vim.api.nvim_create_augroup('mango-vert-help', { clear = true }),
	buffer = 0,
	callback = function()
		vim.cmd.wincmd 'L'
	end,
})
