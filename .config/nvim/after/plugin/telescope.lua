local os = require('os')
local builtin = require('telescope.builtin')
local lib = require('mango.lib')

lib.remap('n', '<leader>pf', function()
	if os.execute('git rev-parse --is-inside-work-tree') == 0 then
		builtin.git_files()
	else
		builtin.find_files()
	end
end)

lib.remap('n', '<leader>ps', function()
	builtin.grep_string({ search = vim.fn.input('Grep > ') })
end)
