local M = {}

function M.set_tab_width(tw, localp)
	local opt = localp and vim.opt_local or vim.opt

	opt.tabstop = tw
	opt.softtabstop = tw
	opt.shiftwidth = tw
end

function M.remap(modes, from, to, opts)
	local ct = {}

	modes:gsub('.', function(c)
		table.insert(ct, c)
	end)

	vim.keymap.set(ct, from, to, opts)
end

return M
