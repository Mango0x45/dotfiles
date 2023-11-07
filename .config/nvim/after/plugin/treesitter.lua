local config = require('nvim-treesitter.parsers').get_parser_configs()
config.gsp = {
	install_info = {
		url = 'https://git.sr.ht/~mango/tree-sitter-gsp',
		files = {'src/parser.c'},
	},
	filetype = 'gsp',
}
config.hypr = {
	install_info = {
		url = 'https://github.com/luckasRanarison/tree-sitter-hypr',
		files = {'src/parser.c'},
	},
	filetype = 'hypr',
}

require('nvim-treesitter.configs').setup {
	ensure_installed = {
		'c',
		'go',
		'gomod',
		'gsp',
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
				['ab'] = '@block.outer',
				['ib'] = '@block.inner',
				['ac'] = '@comment.outer',
				['ic'] = '@comment.inner',
				['af'] = '@function.outer',
				['if'] = '@function.inner',
				['an'] = '@node.outer',
				['in'] = '@node.inner',
			},
		},
		move = {
			enable = true,
			set_jumps = true,
			goto_next_start = {
				[']c'] = '@comment.outer',
				[']f'] = '@function.outer',
				[']b'] = '@block.outer',
				[']n'] = '@node.outer',
			},
			goto_next_end = {
				[']C'] = '@comment.outer',
				[']F'] = '@function.outer',
				[']B'] = '@block.outer',
				[']N'] = '@node.outer',
			},
			goto_previous_start = {
				['[c'] = '@comment.outer',
				['[f'] = '@function.outer',
				['[b'] = '@block.outer',
				['[n'] = '@node.outer',
			},
			goto_previous_end = {
				['[C'] = '@comment.outer',
				['[F'] = '@function.outer',
				['[B'] = '@block.outer',
				['[N'] = '@node.outer',
			},
		},
	},
}
