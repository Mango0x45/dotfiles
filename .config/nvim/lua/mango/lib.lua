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

function M.save_regs(regs, callback)
	local rs = {}
	for r in regs:gmatch('.') do
		rs[r] = {
			s = vim.fn.getreg(r),
			t = vim.fn.getregtype(r),
		}
	end

	callback()

	for k, v in pairs(rs) do
		vim.fn.setreg(k, v.s, v.t)
	end
end

return M
