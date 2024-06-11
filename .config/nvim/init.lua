vim.g.have_nerd_font = false
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
vim.g.netrw_banner = 0
vim.g.netrw_bufsettings = 'noma nomod nu nobl nowrap ro'
vim.g.netrw_list_hide = [[^\(\.\|\.\.\)/\?$,^__pycache__/\?,.*\.\(a\|o\|so\|pyc\)$]]

-- NOTE: :help option-list
vim.opt.backup = false
vim.opt.breakindent = true
vim.opt.cursorline = true
vim.opt.expandtab = false
vim.opt.exrc = true
vim.opt.grepprg = 'rg --vimgrep -uu'
vim.opt.hlsearch = true
vim.opt.ignorecase = true
vim.opt.inccommand = 'split'
vim.opt.list = false
vim.opt.listchars = { tab = '» ', trail = '·', nbsp = '␣' }
vim.opt.matchpairs:append('<:>')
vim.opt.matchpairs:append('‘:’')
vim.opt.matchpairs:append('“:”')
vim.opt.mouse = 'a'
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.scrolloff = 8
vim.opt.secure = true
vim.opt.shiftwidth = 4
vim.opt.showmode = false
vim.opt.signcolumn = 'no'
vim.opt.smartcase = true
vim.opt.smartindent = true
vim.opt.softtabstop = 4
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.swapfile = false
vim.opt.tabstop = 4
vim.opt.undodir = os.getenv('XDG_STATE_HOME') .. '/nvim/undo'
vim.opt.undofile = true

function mk_scratch_buffer()
	vim.cmd [[
		if bufexists('scratch')
			buffer scratch
		else
			noswapfile hide enew
			setlocal buftype=nofile bufhidden=hide
			file scratch
		endif
	]]
end

vim.keymap.set('x', '<leader>a', 'gg0oG$',
	{ desc = 'Select the [A]ll of the buffer' })
vim.keymap.set('o', '<leader>a', ':normal! ggVG<CR>',
	{ desc = 'Text object of [A]ll of the buffer' })
vim.keymap.set('n', '<C-h>', '<C-w><C-h>',
	{ desc = 'Move focus to the left window' })
vim.keymap.set('n', '<C-j>', '<C-w><C-j>',
	{ desc = 'Move focus to the lower window' })
vim.keymap.set('n', '<C-k>', '<C-w><C-k>',
	{ desc = 'Move focus to the upper window' })
vim.keymap.set('n', '<C-l>', '<C-w><C-l>',
	{ desc = 'Move focus to the right window' })
vim.keymap.set('n', '<C-v>', 'V',
	{ desc = 'Enter visual-line mode' })
vim.keymap.set('n', '<leader>h', function() vim.cmd 'split' end,
	{ desc = 'Open a [H]orizontal split' })
vim.keymap.set('n', '<leader>v', function() vim.cmd 'vsplit' end,
	{ desc = 'Open a [V]ertical split' })
vim.keymap.set('n', 'M', ':w! | make<CR>',
	{ desc = 'Run the configured compiler' })
vim.keymap.set('n', 'V', '<C-v>',
	{ desc = 'Enter visual-block mode' })
vim.keymap.set('x', '<C-j>', ":m '>+1<CR>gv=gv",
	{ desc = 'Move a selection down a line' })
vim.keymap.set('x', '<C-k>', ":m '<-2<CR>gv=gv",
	{ desc = 'Move a selection up a line' })
vim.keymap.set({ 'n', 'o', 'x' }, '€', '$',
	{ desc = 'Go to end of the line' })
vim.keymap.set('n', '<Esc>', '<Cmd>nohlsearch<CR>',
	{ desc = 'Disable highlighting of currently highlighted search matches' })
vim.keymap.set('n', '<leader>t', function() vim.cmd.normal('"zx"zph') end,
	{ desc = '[T]ranspose the current and next characters' })
vim.keymap.set('n', '<leader>T', function() vim.cmd.normal('"zX"zp') end,
	{ desc = '[T]ranspose the current and previous characters' })
vim.keymap.set('n', '-', ':Ex<CR>',
	{ desc = 'Open Netrw' })
vim.keymap.set('n', 'gJ', function()
	vim.cmd [[
		let save = winsaveview()
		normal! gJ
		if matchstr(getline('.'), '\%' . col('.') . 'c.') =~ '\s'
			normal! "_dw
		endif
		call winrestview(save)
	]]
end, { desc = '[J]oin lines without whitespace' })

vim.keymap.set('n', '<leader>k', function()
	vim.cmd 'vsplit'
	mk_scratch_buffer()
end, { desc = 'Open the scratch buffer in a vertical split' })
vim.keymap.set('n', '<leader>K', function()
	vim.cmd 'split'
	mk_scratch_buffer()
end, { desc = 'Open the scratch buffer in a horizontal split' })

-- Don’t move the cursor with various commands
vim.keymap.set('n', 'J', 'mzJ`z')
vim.keymap.set('n', '<C-d>', '<C-d>zz')
vim.keymap.set('n', '<C-u>', '<C-u>zz')
vim.keymap.set('n', 'n', 'nzzzv')
vim.keymap.set('n', 'N', 'Nzzzv')

vim.api.nvim_create_autocmd('BufEnter', {
	desc = 'Disable auto-commenting',
	group = vim.api.nvim_create_augroup('mango-no-autocomment', { clear = true }),
	callback = function()
		vim.opt.formatoptions:remove({ 'c', 'r', 'o' })
	end,
})

vim.api.nvim_create_autocmd('TextYankPost', {
	desc = 'Momentarily highlight yanked text',
	group = vim.api.nvim_create_augroup('mango-highlight-yank', { clear = true }),
	callback = function()
		vim.highlight.on_yank()
	end,
})

-- Weird way to make buffer auto-reverting work?
vim.cmd [[
	if !exists('g:CheckUpdateStarted')
		let g:CheckUpdatedStarted = 1
		call timer_start(1000, 'CheckUpdate', {'repeat': -1})
	endif

	function! CheckUpdate(_)
		silent! checktime
	endfunction
]]

-- Bootstrap Paq
local paqpath = vim.fn.stdpath('data') .. '/site/pack/paqs/start/paq-nvim'
if not vim.uv.fs_stat(paqpath) then
	vim.fn.system({
		'git', 'clone', '--depth=1',
		'https://github.com/savq/paq-nvim.git',
		paqpath
	})
end

require 'paq' {
	'christoomey/vim-sort-motion',
	'echasnovski/mini.nvim',
	'folke/todo-comments.nvim',
	'folke/tokyonight.nvim',
	'hrsh7th/cmp-nvim-lsp',
	'hrsh7th/cmp-path',
	'hrsh7th/nvim-cmp',
	'https://git.sr.ht/~mango/tree-sitter-gsp',
	'kylechui/nvim-surround',
	'L3MON4D3/LuaSnip',
	'luckasRanarison/tree-sitter-hypr',
	'mattn/emmet-vim',
	'neovim/nvim-lspconfig',
	'nvim-lua/plenary.nvim',
	'nvim-telescope/telescope.nvim',
	'nvim-telescope/telescope-ui-select.nvim',
	{ 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },
	'nvim-treesitter/nvim-treesitter-textobjects',
	'saadparwaiz1/cmp_luasnip',
	'savq/paq-nvim',
	'wellle/targets.vim',
}

-- emmet-vim
vim.g.user_emmet_install = false

-- vim-sort-motion
vim.g.sort_motion_flags = 'l'

-- tokyonight.nvim
vim.cmd.colorscheme 'tokyonight-night'

-- telescope.nvim
local telescope = require 'telescope'
local tsactions = require 'telescope.actions'
local tsbuiltin = require 'telescope.builtin'
local tsthemes  = require 'telescope.themes'

telescope.setup {
	defaults = {
		scroll_strategy = 'limit',
		path_display = { 'filename_first' },
		get_status_text = function(_) return '' end,
		mappings = {
			i = {
				['<Esc>'] = {
					tsactions.close,
					type = 'action',
					opts = { nowait = true, silent = true },
				},
				['<C-j>'] = {
					tsactions.move_selection_next,
					type = 'action',
					opts = { nowait = true, silent = true },
				},
				['<C-k>'] = {
					tsactions.move_selection_previous,
					type = 'action',
					opts = { nowait = true, silent = true },
				},
			},
		},
	},
	extensions = {
		['ui-select'] = { tsthemes.get_dropdown() },
	}
}

pcall(telescope.load_exetension, 'fzf')
pcall(telescope.load_exetension, 'ui-select')

vim.keymap.set('n', '<leader>ff', tsbuiltin.find_files,
	{ desc = '[F]ind [F]iles' })
vim.keymap.set('n', '<leader>fh', tsbuiltin.help_tags,
	{ desc = '[F]ind [H]elp' })
vim.keymap.set('n', '<leader>fg', tsbuiltin.live_grep,
	{ desc = '[F]ind [G]rep' })
vim.keymap.set('n', '<leader>/', function()
	tsbuiltin.current_buffer_fuzzy_find(tsthemes.get_dropdown {
		winblend = 10,
		previewer = false,
	})
end, { desc = 'Fuzzily search in current buffer' })

-- todo-comments.nvim
require('todo-comments').setup {
	signs = false,
	keywords = {
		TODO = { color = 'info' },
		NOTE = { color = 'hint' },
	},
	highlight = {
		before = '',
		keyword = 'fg',
		after = '',
	},
}

-- mini.nvim
local statusline = require 'mini.statusline'
statusline.setup { use_icons = false }
statusline.section_location = function() return '%2l:%-2v' end

-- nvim-treesitter
local treeconfs = require('nvim-treesitter.parsers').get_parser_configs()
treeconfs.gsp = {
	install_info = {
		url = 'https://git.sr.ht/~mango/tree-sitter-gsp',
		files = { 'src/parser.c' },
	},
	filetype = 'gsp',
}

require('nvim-treesitter.install').prefer_git = true
require('nvim-treesitter.configs').setup {
	auto_install = true,
	sync_install = true,

	highlight = {
		enable = true,
		additional_vim_regex_highlighting = false,
	},

	incremental_selection = {
		enable = true,
		keymaps = {
			init_selection   = '<C-Space>',
			node_incremental = '<C-Space>',
			node_decremental = '<C-s>',
		},
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
				['at'] = '@text.outer',
				['it'] = '@text.outer',
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
				[']t'] = '@text.outer',
			},
			goto_next_end = {
				[']C'] = '@comment.outer',
				[']F'] = '@function.outer',
				[']B'] = '@block.outer',
				[']N'] = '@node.outer',
				[']T'] = '@text.outer',
			},
			goto_previous_start = {
				['[c'] = '@comment.outer',
				['[f'] = '@function.outer',
				['[b'] = '@block.outer',
				['[n'] = '@node.outer',
				['[t'] = '@text.outer',
			},
			goto_previous_end = {
				['[C'] = '@comment.outer',
				['[F'] = '@function.outer',
				['[B'] = '@block.outer',
				['[N'] = '@node.outer',
				['[T'] = '@text.outer',
			},
		},
	},
}

-- nvim-surround
require('nvim-surround').setup {
	surrounds = {
		['’'] = {
			add = { '‘', '’' },
			find = '‘[^‘’]*’',
			delete = '^(‘)().-(’)()$',
		},

		['‘'] = {
			add = { '‘ ', ' ’' },
			find = '‘[^‘’]*’',
			delete = '^(‘ *)().-( *’)()$',
		},

		['”'] = {
			add = { '“', '”' },
			find = '“[^“”]*”',
			delete = '^(“)().-(”)()$',
		},

		['“'] = {
			add = { '“ ', ' ”' },
			find = '“[^“”]*”',
			delete = '^(“ *)().-( *”)()$',
		},
	}
}

-- nvim-lspconfig
local lsp = require 'lspconfig'
local caps = require('cmp_nvim_lsp').default_capabilities()

lsp.clangd.setup {
	cmd = { 'clangd', '-header-insertion=never' },
	capabilities = caps,
}
lsp.gopls.setup {
	capabilities = caps,
}
lsp.rust_analyzer.setup {
	capabilities = caps,
}
lsp.lua_ls.setup {
	settings = {
		Lua = {
			runtime = {
				version = 'LuaJIT',
			},
			diagnostics = {
				globals = {
					'vim',
					'require',
				},
			},
			workspace = {
				library = vim.api.nvim_get_runtime_file('', true),
			},
			telemetry = {
				enable = false,
			},
		},
	},
	capabilities = caps,
}

vim.api.nvim_create_autocmd('LspAttach', {
	group = vim.api.nvim_create_augroup('mango-lsp-config', { clear = true }),
	callback = function(ev)
		local function remap(mode, map, fn)
			vim.keymap.set(mode, map, fn, { buffer = ev.buf })
		end

		vim.diagnostic.disable()
		vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'
		vim.keymap.set('n', 'gK', vim.lsp.buf.hover,
			{ buffer = ev.buf, desc = 'View symbol hover information' })
		vim.keymap.set('n', 'gd', vim.lsp.buf.definition,
			{ buffer = ev.buf, desc = 'Goto [D]efinition' })
		vim.keymap.set('n', 'gi', vim.lsp.buf.implementation,
			{ buffer = ev.buf, desc = 'Goto [I]mplementation' })
		vim.keymap.set('n', 'gr', vim.lsp.buf.rename,
			{ buffer = ev.buf, desc = '[R]ename symbol' })
		vim.keymap.set('n', 'gt', vim.lsp.buf.type_definition,
			{ buffer = ev.buf, desc = 'Goto [T]ype definition' })
	end,
})

-- nvim-cmp & luasnip
local cmp = require 'cmp'
local luasnip = require 'luasnip'

luasnip.config.setup {
	history = true,
	updateevents = 'TextChanged,TextChangedI',
}
require 'snippets'

cmp.setup {
	snippet = {
		expand = function(args)
			luasnip.lsp_expand(args.body)
		end,
	},
	completion = { completeopt = 'menu,menuone,noinsert' },
	mapping = cmp.mapping.preset.insert {
		['<CR>'] = cmp.mapping.confirm { select = true },
		['<C-Space>'] = cmp.mapping.complete(),
		['<C-p>'] = cmp.mapping.scroll_docs(-1),
		['<C-n>'] = cmp.mapping.scroll_docs(1),
		['<C-c>'] = cmp.mapping.abort(),
		['<C-j>'] = cmp.mapping.select_next_item({
			behaviour = cmp.SelectBehavior.Select,
		}),
		['<C-k>'] = cmp.mapping.select_prev_item({
			behaviour = cmp.SelectBehavior.Select,
		}),
		['<C-l>'] = cmp.mapping(function()
			if luasnip.expand_or_locally_jumpable() then
				luasnip.expand_or_jump()
			end
		end, {'i', 's'}),
		['<C-h>'] = cmp.mapping(function()
			if luasnip.expand_or_locally_jumpable(-1) then
				luasnip.expand_or_jump(-1)
			end
		end, {'i', 's'}),
	},
	sources = {
		{ name = 'nvim_lsp' },
		{ name = 'luasnip'  },
		{ name = 'path'     },
	},
	experimental = {
		ghost_text = true,
	},
}
-- cmp.event:on('confirm_done', function(ev)
-- 	if ev.commit_character then
-- 		return
-- 	end
--
-- 	local types = require 'cmp.types'
-- 	local kind = ev.entry:get_kind()
--
-- 	if
-- 		kind == types.lsp.CompletionItemKind.Function or
-- 		kind == types.lsp.CompletionItemKind.Method
-- 	then
-- 		local keys = vim.api.nvim_replace_termcodes('()<left>', true, false, true)
-- 		vim.api.nvim_feedkeys(keys, 'i', false)
-- 	end
-- end)

require('pipe')
