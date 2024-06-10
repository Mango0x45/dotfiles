function pipe_visual()
	pipe_internal({'<', '>'}, vim.fn.visualmode() ~= 'v')
end

function pipe_normal(arg)
	pipe_internal({'[', ']'}, arg == 'line')
end

function pipe_internal(marks, linesp)
	local mode = vim.fn.visualmode()

	local sr, sc = unpack(vim.api.nvim_buf_get_mark(0, marks[1]))
	local er, ec = unpack(vim.api.nvim_buf_get_mark(0, marks[2]))

	local lines = linesp
		and vim.api.nvim_buf_get_lines(0, sr - 1, er, true)
		or vim.api.nvim_buf_get_text(0, sr - 1, sc, er - 1, ec + 1, {})

	local ok, cmd = pcall(vim.fn.input, {
		prompt = '… | ',
		cancelreturn = vim.NIL,
	})

	if not ok or cmd == vim.NIL then
		return
	end

	local out = vim.fn.systemlist(cmd, lines)
	if linesp then
		vim.api.nvim_buf_set_lines(0, sr - 1, er, true, out)
	else
		vim.api.nvim_buf_set_text(0, sr - 1, sc, er - 1, ec + 1, out)
	end
end

vim.keymap.set('n', '<Plug>PipeNormal', ':set opfunc=v:lua.pipe_normal<CR>g@',
	{ silent = true })
vim.keymap.set('x', '<Plug>PipeVisual', ':<C-u>lua pipe_visual()<CR>',
	{ silent = true })

vim.keymap.set('n', '|', '<Plug>PipeNormal')
vim.keymap.set('x', '|', '<Plug>PipeVisual')
