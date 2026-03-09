vim.g.have_nerd_font = false
vim.g.mapleader = ' '
vim.g.maplocalleader = ','
vim.g.netrw_banner = 0
vim.g.netrw_bufsettings = 'noma nomod nu nobl nowrap ro'
vim.g.netrw_list_hide = [[^\(\.\|\.\.\)/\?$,^__pycache__/\?,.*\.\(a\|o\|so\|pyc\)$]]

-- NOTE: :help option-list
vim.opt.backup = false
vim.opt.breakindent = true
vim.opt.conceallevel = 2
vim.opt.cursorline = true
vim.opt.expandtab = false
vim.opt.exrc = true
vim.opt.grepprg = 'rg --vimgrep'
vim.opt.hlsearch = true
vim.opt.ignorecase = true
vim.opt.inccommand = 'split'
vim.opt.list = false
vim.opt.listchars = { tab = '» ', trail = '․', nbsp = '␣' }
vim.opt.matchpairs:append('<:>')
vim.opt.matchpairs:append('‘:’')
vim.opt.matchpairs:append('“:”')
vim.opt.matchpairs:append('»:«')
vim.opt.matchpairs:append('›:‹')
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

vim.keymap.set('n', '<C-j>', '<C-w><C-j>',
	{ desc = 'Move focus to the lower window' })
vim.keymap.set('n', '<C-k>', '<C-w><C-k>',
	{ desc = 'Move focus to the upper window' })
vim.keymap.set('n', '<C-h>', ':WinMovePrev<CR>',
	{ desc = 'Move focus to the left window', silent = true })
vim.keymap.set('n', '<C-l>', ':WinMoveNext<CR>',
	{ desc = 'Move focus to the right window', silent = true })
vim.keymap.set('n', '<Leader>h', function() vim.cmd 'split' end,
	{ desc = 'Open a [H]orizontal split' })
vim.keymap.set('n', '<Leader>v', function() vim.cmd 'vsplit' end,
	{ desc = 'Open a [V]ertical split' })
vim.keymap.set('n', 'M', ':w! | make<CR>',
	{ desc = 'Run the configured compiler', silent = true })
vim.keymap.set('x', '<C-j>', ":m '>+1<CR>gv=gv",
	{ desc = 'Move a selection down a line', silent = true })
vim.keymap.set('x', '<C-k>', ":m '<-2<CR>gv=gv",
	{ desc = 'Move a selection up a line', silent = true })
vim.keymap.set({ 'n', 'o', 'x' }, '€', '$',
	{ desc = 'Go to end of the line' })
vim.keymap.set('n', '<Esc>', ':nohlsearch<CR>',
	{ desc = 'Disable highlighting of currently highlighted search matches',
	  silent = true })
vim.keymap.set('n', '<Leader>t', function() vim.cmd.normal('"zx"zph') end,
	{ desc = '[T]ranspose the current and next characters' })
vim.keymap.set('n', '<Leader>T', function() vim.cmd.normal('"zX"zp') end,
	{ desc = '[T]ranspose the current and previous characters' })
vim.keymap.set('n', '-', ':Ex<CR>',
	{ desc = 'Open Netrw', silent = true })
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

vim.keymap.set('n', '<Leader>s', ':VScratch<CR>',
	{ desc = 'Open the scratch buffer in a vertical split' })
vim.keymap.set('n', '<Leader>S', ':Scratch<CR>',
	{ desc = 'Open the scratch buffer in a horizontal split' })

if vim.loop.os_uname().sysname ~= 'Darwin' then
	vim.keymap.set({'n', 'x'}, '<C-v>', 'V',
		{ desc = 'Enter visual-line mode' })
	vim.keymap.set({'n', 'x'}, 'V', '<C-v>',
		{ desc = 'Enter visual-block mode' })
end

-- Don’t move the cursor with various commands
vim.keymap.set('n', 'J', 'mzJ`z')
vim.keymap.set('n', '<C-d>', '<C-d>zz')
vim.keymap.set('n', '<C-u>', '<C-u>zz')
vim.keymap.set('n', 'n', 'nzzzv')
vim.keymap.set('n', 'N', 'Nzzzv')

vim.api.nvim_create_autocmd('BufEnter', {
	desc = 'Disable auto-commenting',
	group = vim.api.nvim_create_augroup('mango-no-autocomment',
		{ clear = true }),
	callback = function()
		vim.opt.formatoptions:remove({ 'c', 'r', 'o' })
	end,
})

vim.api.nvim_create_autocmd('TextYankPost', {
	desc = 'Momentarily highlight yanked text',
	group = vim.api.nvim_create_augroup('mango-highlight-yank',
		{ clear = true }),
	callback = function()
		vim.highlight.on_yank()
	end,
})

vim.api.nvim_create_autocmd('VimEnter', {
	desc = 'Remove the Vim background color',
	group = vim.api.nvim_create_augroup('mango-highlight-config',
		{ clear = true }),
	callback = function()
		vim.cmd [[
			highlight Comment               cterm=NONE gui=NONE
			highlight CursorColumn          guibg=#1D2635
			highlight CursorLine            guibg=#1D2635
			highlight EndOfBuffer           guibg=NONE
			highlight Normal                guibg=NONE
			highlight NormalNC              guibg=NONE
			highlight StatusLine            guibg=#19212E
			highlight TabLineFill           guibg=#19212E
			highlight TabLine               guibg=#131A25
			highlight TelescopeBorder       guibg=NONE
			highlight TelescopeNormal       guibg=NONE
			highlight TelescopePromptBorder guibg=NONE
			highlight TelescopePromptTitle  guibg=NONE
		]]
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
	'folke/todo-comments.nvim',
	'folke/tokyonight.nvim',
	'kylechui/nvim-surround',
	'luckasRanarison/tree-sitter-hypr',
	'Mango0x45/tree-sitter-gsp',
	'ngalaiko/tree-sitter-go-template',
	'nvim-lua/plenary.nvim',
	{
		'nvim-treesitter/nvim-treesitter',
		branch = 'master',
		build = ':TSUpdate',
	},
	'nvim-treesitter/nvim-treesitter-textobjects',
	'savq/paq-nvim',
	'wellle/targets.vim',
}

-- tokyonight.nvim
vim.cmd.colorscheme 'tokyonight-night'

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

-- nvim-treesitter
local treeconfs = require('nvim-treesitter.parsers').get_parser_configs()
treeconfs.gsp = {
	install_info = {
		url = 'https://github.com/Mango0x45/tree-sitter-gsp',
		files = { 'src/parser.c' },
	},
	filetype = 'gsp',
}
treeconfs.gotmpl = {
	install_info = {
		url = 'https://github.com/ngalaiko/tree-sitter-go-template',
		files = { 'src/parser.c' },
	},
	filetype = 'gotmpl',
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
local surround_conf = require('nvim-surround.config')
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

		['l'] = {
			add = function()
				local result = surround_conf.get_input('Array name: ')
				if result then
					return { { result .. '[' }, { ']' } }
				end
			end,
			find = function()
				return surround_conf.get_selection({
					pattern = '[^=%s%(%){}]+%b[]'
				})
			end,
			delete = '^(.-%[)().-(%])()$',
			change = {
				target = '^.-([%w_]+)()%[.-%]()()$',
				replacement = function()
					local result = surround_conf.get_input('Array name: ')
					if result then
						return { { result }, { '' } }
					end
				end,
			},
		},
	}
}

-- Pipe
require('pipe')
