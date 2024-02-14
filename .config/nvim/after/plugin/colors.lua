local nohl = { bg = "surface" }

require('rose-pine').setup {
	styles = {
		transparency = true,
	},
	highlight_groups = {
		Pmenu = nohl,
		StatusLine = nohl,
	},
}

vim.cmd.colorscheme('rose-pine')
