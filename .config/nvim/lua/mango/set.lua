local lib = require('mango.lib')
local api = vim.api

local o = vim.opt
local g = vim.g

local augroup = api.nvim_create_augroup('Mango', { clear = true })

o.nu = true
o.relativenumber = true
o.signcolumn = 'no'
o.colorcolumn = '81'

o.exrc = true
o.secure = true

g.guifont = { 'Iosevka Smooth Term', ':h16' }

lib.set_tab_width(4)
o.expandtab = false
o.smartindent = true

o.wrap = false

o.swapfile = false
o.backup = false
o.undodir = os.getenv('XDG_STATE_HOME') .. '/nvim/undo'
o.undofile = true

o.hlsearch = true
o.incsearch = true

o.termguicolors = true

o.scrolloff = 8

o.isfname:append('@-@')

o.splitright = true
o.splitbelow = true

-- Better settings for netrw
g.netrw_banner = 0
g.netrw_bufsettings = 'noma nomod nu nobl nowrap ro' -- Enable line-numbers
g.netrw_list_hide = [[^\(\.\|\.\.\)/\?$,^__pycache__/\?,.*\.\(o\|pyc\)$]]

vim.cmd [[ set grepprg=rg\ --vimgrep ]]

-- Disable auto commenting
api.nvim_create_autocmd('BufEnter', {
	callback = function()
		o.formatoptions:remove({ 'c', 'r', 'o' })
	end,
	group = augroup,
})

-- Make buffer auto-reverting work… somehow
vim.cmd([[
	if !exists('g:CheckUpdateStarted')
		let g:CheckUpdateStarted = 1
		call timer_start(1000, 'CheckUpdate', {'repeat': -1})
	endif

	function! CheckUpdate(_)
		silent! checktime
	endfunction
]])

-- Allow for jumping between these pairs with %
o.matchpairs:append('<:>')
o.matchpairs:append('‘:’')
o.matchpairs:append('“:”')
