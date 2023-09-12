local lib = require('mango.lib')
local api = vim.api
local opt = vim.opt

opt.nu = true
opt.relativenumber = true

lib.setTabWidth(4)
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
local group = api.nvim_create_augroup('Mango', { clear = true })
api.nvim_create_autocmd(
	'BufEnter',
	{
		callback = function()
			opt.formatoptions:remove({'c', 'r', 'o'})
		end,
		group = group,
	}
)
