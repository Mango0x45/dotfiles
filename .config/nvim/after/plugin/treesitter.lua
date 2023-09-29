local config = require('nvim-treesitter.parsers').get_parser_configs()
config.gsp = {
	install_info = {
		url = 'https://git.sr.ht/~mango/tree-sitter-gsp',
		files = {'src/parser.c'},
	},
	filetype = 'gsp',
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
				['af'] = '@function.outer',
				['if'] = '@function.inner',
				['ab'] = '@block.outer',
				['ib'] = '@block.inner',
				['an'] = '@node.outer',
				['in'] = '@node.inner',
			},
		},
		move = {
			enable = true,
			set_jumps = true,
			goto_next_start = {
				[']f'] = '@function.outer',
				[']b'] = '@block.outer',
				[']n'] = '@node.outer',
			},
			goto_next_end = {
				[']F'] = '@function.outer',
				[']B'] = '@block.outer',
				[']N'] = '@node.outer',
			},
			goto_previous_start = {
				['[f'] = '@function.outer',
				['[b'] = '@block.outer',
				['[n'] = '@node.outer',
			},
			goto_previous_end = {
				['[F'] = '@function.outer',
				['[B'] = '@block.outer',
				['[N'] = '@node.outer',
			},
		},
	},
}
