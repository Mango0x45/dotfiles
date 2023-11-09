local ts_utils = require('nvim-treesitter.ts_utils')

local function map(lhs, rhs)
	vim.keymap.set('n', lhs, function()
		local node = ts_utils.get_node_at_cursor()
		if node == nil then
			error('No tree-sitter parser found.')
		end

		while node ~= nil and node:type() ~= 'operation' do
			node = node:parent()
		end

		if node == nil or node:type() ~= 'operation' then
			return
		end

		local sr, sc, er, ec = node:child(0):range()
		vim.api.nvim_buf_set_text(0, sr, sc, er, ec, { rhs })
	end, {
		buffer = true,
		noremap = true,
		silent = true,
	})
end

map('p', 'pick')
map('r', 'reword')
map('s', 'squash')
map('f', 'fixup')
