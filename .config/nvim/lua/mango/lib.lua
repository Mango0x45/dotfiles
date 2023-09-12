local M = {}

function M.set_tab_width(tw, localp)
	local opt = localp and vim.opt_local or vim.opt

	opt.tabstop = tw
	opt.softtabstop = tw
	opt.shiftwidth = tw
end

return M
