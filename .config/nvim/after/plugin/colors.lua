require('rose-pine').setup({
	disable_background = true
})

function SetTheme(color)
	local hl = vim.api.nvim_set_hl
	vim.cmd.colorscheme(color or 'rose-pine')
	vim.g.neovide_transparency = 0.9
	hl(0, 'Normal', vim.g.neovide and {
		fg = '#C5C8C6',
		bg = '#2B303B',
	} or {
		fg = 'none',
		bg = 'none',
	})

	hl(0, 'NormalFloat', { bg = 'none' })
	hl(0, 'LineNr', { fg = 'yellow' })
	hl(0, 'LineNrAbove', { fg = 'grey' })
	hl(0, 'LineNrBelow', { fg = 'grey' })
end

SetTheme()
