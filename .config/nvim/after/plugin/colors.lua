require('rose-pine').setup({
	disable_background = true
})

function SetTheme(color)
	local hl = vim.api.nvim_set_hl
	vim.cmd.colorscheme(color or 'rose-pine')
	hl(0, 'Normal', vim.g.neovide and {
		fg = '#C5C8C6',
		bg = '#2B303B',
	} or {
		fg = 'none',
		bg = 'none',
	})

	hl(0, 'NormalFloat', { bg = 'none' })
end

SetTheme()
