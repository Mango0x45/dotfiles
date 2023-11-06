local lib = require('mango.lib')
local api = vim.api

local o = vim.opt
local g = vim.g

local augroup = api.nvim_create_augroup('Mango', { clear = true })

o.nu = true
o.relativenumber = true

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
-- TODO: Research
-- o.signcolumn = 'yes'
-- o.isfname:append('@-@')

o.updatetime = 50

o.colorcolumn = '81'

o.splitright = true
o.splitbelow = true

-- Disable auto commenting
api.nvim_create_autocmd('BufEnter', {
	callback = function()
		o.formatoptions:remove({ 'c', 'r', 'o' })
	end,
	group = augroup,
})

-- Make buffer auto-reverting work… somehow
api.nvim_create_autocmd('FocusGained', {
	command = 'checktime',
	group = augroup,
})

-- This has to be done to enable linenumbers in netrw
g.netrw_bufsettings = 'noma nomod nu nobl nowrap ro'

glo.user_emmet_install = false

opt.matchpairs:append('‘:’')
opt.matchpairs:append('“:”')
