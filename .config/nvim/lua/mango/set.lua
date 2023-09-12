local lib = require('mango.lib')
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

-- Disable auto commenting
vim.bo.formatoptions = 'jnql'

opt.splitright = true
opt.splitbelow = true
