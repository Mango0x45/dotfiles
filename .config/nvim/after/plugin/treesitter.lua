require('nvim-treesitter.configs').setup {
	ensure_installed = {
		'c',
		'go',
		'gomod',
		'html',
		'lua',
		'python',
		'query',
		'rust',
		'vim',
		'vimdoc',
		'zig',
	},

	sync_install = false,
	auto_install = true,

	highlight = {
		enable = true,
		additional_vim_regex_highlighting = false,
	},
}
