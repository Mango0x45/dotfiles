local nohl = { bg = "surface" }

require('rose-pine').setup {
	styles = {
		transparency = true,
	},
	highlight_groups = {
		Pmenu = nohl,
		StatusLine = nohl,
		StatusLineNC = nohl,
	},
}

vim.cmd.colorscheme('rose-pine')
vim.api.nvim_set_hl(0, '@lsp.type.macro', { link = 'Macro' })
