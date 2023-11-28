local lib = require('mango.lib')
local remap = lib.remap

vim.g.mapleader = ' '

-- Make adjustments for my custom keyboard layout
remap('nov', '€', '$')

-- Better frame navigation
remap('n', '<C-h>', '<C-w>h')
remap('n', '<C-j>', '<C-w>j')
remap('n', '<C-k>', '<C-w>k')
remap('n', '<C-l>', '<C-w>l')

-- I prefer visual-line mode on ‘V’
remap('n', 'V', '<C-v>')
remap('n', '<C-v>', 'V')

-- Move selections up and down
remap('v', '<C-k>', ":m '<-2<CR>gv=gv")
remap('v', '<C-j>', ":m '>+1<CR>gv=gv")

-- Don’t move cursor with various commands
remap('n', 'J', 'mzJ`z')
remap('n', '<C-d>', '<C-d>zz')
remap('n', '<C-u>', '<C-u>zz')
remap('n', 'n', 'nzzzv')
remap('n', 'N', 'Nzzzv')

-- Swap the jump-to-mark bindings
remap('nv', "'", '`')
remap('nv', '`', "'")

-- Transpose characters; for some reason it has a noticable delay if I just give
-- a string instead of lambda functions
remap('n', '<leader>t', function() vim.cmd.normal('"zx"zp') end)
remap('n', '<leader>T', function() vim.cmd.normal('"zX"zp') end)

-- Open netrw quickly
remap('n', '-', ':Ex<CR>')

-- Open netrw in a vertical- or horizontal split.  The split is made manually
-- instead of by using :Vexplore or :Sexplore so that it’s made on the right or
-- bottom instead of the left or top
remap('n', '–', ':vsplit | Ex<CR>')
remap('n', 'g–', ':split | Ex<CR>')

remap('n', '<leader>s', function()
	vim.cmd('vsplit')
	lib.scratch_buffer()
end)
remap('n', '<leader>S', function()
	vim.cmd('split')
	lib.scratch_buffer()
end)
