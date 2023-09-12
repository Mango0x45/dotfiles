require('rose-pine').setup({
	disable_background = true
})

function SetTheme(color)
	vim.cmd.colorscheme(color or 'rose-pine')
	vim.api.nvim_set_hl(0, 'Normal',      { bg = 'none' })
	vim.api.nvim_set_hl(0, 'NormalFloat', { bg = 'none' })
end

SetTheme()
