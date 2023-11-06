local lib = require('mango.lib')
local remap = lib.remap

vim.g.mapleader = ' '

-- Make adjustments for my custom keyboard layout
remap('nv', '€', '$')

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

-- Paste and delete without clobbering primary register
remap('x', '<leader>p', '"_dP')
remap('nv', '<leader>d', '"_d')

-- Copy to system clipboard
remap('nv', '<leader>y', '"+y')
remap('n', '<leader>Y', '"+Y')

-- Paste from system clipboard
remap('i', '<C-+>', '"+pa')

-- Swap the jump-to-mark bindings
remap('nv', "'", '`')
remap('nv', '`', "'")

-- Transpose characters without clobbering registers
local function transpose_chars(rev)
	lib.save_regs('a', function()
		vim.cmd.normal('"a' .. (rev and 'X' or 'x') .. '"ap')
	end)
end

-- Transpose characters
remap('n', '<leader>t', transpose_chars)
remap('n', '<leader>T', function()
	transpose_chars(true)
end)

-- Open netrw in a vertical split
remap('n', '–', function()
	vim.cmd('vsplit | Ex')
end)

-- Open netrw in a horizontal split
remap('n', 'g–', function()
	vim.cmd('split | Ex')
end)

-- Sort lines in selection
remap('v', 's', ':sort<CR>')
