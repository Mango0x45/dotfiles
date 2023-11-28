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

	vim.keymap.set(ct, from, to, opts or {
		noremap = true,
		silent = true,
	})
end

function M.scratch_buffer()
	vim.cmd [[
		if bufexists('scratch')
			buffer scratch
		else
			noswapfile hide enew
			setlocal buftype=nofile bufhidden=hide
			file scratch
		endif
	]]
end

return M
