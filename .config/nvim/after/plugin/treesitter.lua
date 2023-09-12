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

	indent = {
		enable = true,
	},

	incremental_selection = {
		enable = true,
		keymaps = {
			init_selection = '<C-Space>',
			node_incremental = '<C-Space>',
			scope_incremental = '<C-s>',
			node_decremental = '<C-Backspace>',
		}
	},

	textobjects = {
		select = {
			enable = true,
			lookahead = true,
			keymaps = {
				['af'] = '@function.outer',
				['if'] = '@function.inner',
				['ab'] = '@block.outer',
				['ib'] = '@block.inner',
			},
		},
		move = {
			enable = true,
			set_jumps = true,
			goto_next_start = {
				[']f'] = '@function.outer',
				[']b'] = '@block.outer',
			},
			goto_next_end = {
				[']F'] = '@function.outer',
				[']B'] = '@block.outer',
			},
			goto_previous_start = {
				['[f'] = '@function.outer',
				['[b'] = '@block.outer',
			},
			goto_previous_end = {
				['[F'] = '@function.outer',
				['[B'] = '@block.outer',
			},
		},
	},
}
