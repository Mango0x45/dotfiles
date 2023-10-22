local lib = require('mango.lib')
local api = vim.api
local opt = vim.opt
local glo = vim.g

local augroup = api.nvim_create_augroup('Mango', { clear = true })

opt.nu = true
opt.relativenumber = true

opt.exrc = true
opt.secure = true

opt.guifont = { 'Iosevka Smooth Term', ':h16' }

lib.set_tab_width(4)
opt.expandtab = false
opt.smartindent = true

opt.wrap = false

opt.swapfile = false
opt.backup = false
opt.undodir = os.getenv('XDG_STATE_HOME') .. '/nvim/undo'
opt.undofile = true

opt.hlsearch = true
opt.incsearch = true

opt.termguicolors = true

opt.scrolloff = 8
-- TODO: Research
-- opt.signcolumn = 'yes'
-- opt.isfname:append('@-@')

opt.updatetime = 50

opt.colorcolumn = '81'

opt.splitright = true
opt.splitbelow = true

-- Disable auto commenting
api.nvim_create_autocmd('BufEnter', {
	callback = function()
		opt.formatoptions:remove({ 'c', 'r', 'o' })
	end,
	group = augroup,
})

api.nvim_create_autocmd('FocusGained', {
	callback = function()
		vim.cmd('checktime')
	end,
	group = augroup,
})

opt.foldmethod = 'expr'
opt.foldexpr = 'nvim_treesitter#foldexpr()'

api.nvim_create_autocmd('BufWinEnter', {
	command = 'normal zR',
	group = augroup,
})

-- This has to be done to enable linenumbers in netrw
glo.netrw_bufsettings = 'noma nomod nu nobl nowrap ro'

glo.user_emmet_install = false
