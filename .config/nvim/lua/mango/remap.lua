local function remap(modes, from, to)
	modes:gsub('.', function(c)
		vim.keymap.set(c, from, to)
	end)
end

vim.g.mapleader = ' '

-- Make adjustments for my custom keyboard layout
remap('nv', '€', '$')
remap('nv', ')', '0')

-- I only ever shift entire lines
remap('n', '<', '<<')
remap('n', '>', '>>')

-- Open netrw
remap('n', '<leader>rw', vim.cmd.Ex)

-- Better frame navigation
remap('n', '<C-h>', '<C-w>h')
remap('n', '<C-j>', '<C-w>j')
remap('n', '<C-k>', '<C-w>k')
remap('n', '<C-l>', '<C-w>l')

-- I prefer visual-line mode on ‘V’
remap('n', 'V', '<C-v>')
remap('n', '<C-v>', 'V')

-- Move selections up and down
remap('v', '<C-J>', ":m '>+1<CR>gv=gv")
remap('v', '<C-K>', ":m '<-2<CR>gv=gv")

-- Don’t move cursor with various commands
remap('n', 'J', 'mzJ`z')
remap('n', '<C-d>', '<C-d>zz')
remap('n', '<C-u>', '<C-u>zz')
remap('n', 'n', 'nzzzv')
remap('n', 'N', 'Nzzzv')

-- Paste and delete without clobbering primary register
remap('x',  '<leader>p', '"_dP')
remap('nv', '<leader>d', '"_d')

-- Copy to system clipboard
remap('nv', '<leader>y', '"+y')
remap('n',  '<leader>Y', '"+Y')

-- Swap the jump-to-mark bindings
remap('nv', "'", '`')
remap('nv', '`', "'")

-- Transpose characters, because ‘xp’ is kinda awkward
remap('n', '<leader>t', 'xp')
