local builtin = require('telescope.builtin')
local lib = require('mango.lib')

--vim.keymap.set('n', '<leader>pf', builtin.git_files, {})

lib.remap('n', '<leader>pf', builtin.find_files)
lib.remap('n', '<leader>ps', function()
	builtin.grep_string({ search = vim.fn.input('Grep > ') })
end)
