function pipe_visual()
	pipe_internal({'<', '>'}, vim.fn.visualmode())
end

function pipe_normal(mode)
	local map = {
		['char'] = 'v',
		['line'] = 'V',
		['block'] = '\x16',
	}
	pipe_internal({'[', ']'}, map[mode])
end

function pipe_internal(marks, mode)
	local lines
	local sr, sc = unpack(vim.api.nvim_buf_get_mark(0, marks[1]))
	local er, ec = unpack(vim.api.nvim_buf_get_mark(0, marks[2]))

	if mode == 'v' then -- Visual
		lines = vim.api.nvim_buf_get_text(0, sr - 1, sc, er - 1, ec + 1, {})
	elseif mode == 'V' then -- Visual Line
		lines = vim.api.nvim_buf_get_lines(0, sr - 1, er, true)
	elseif mode == '\x16' then -- Visual Block
		if sc > ec then
			sc, ec = ec, sc
		end
		lines = {}
		for i = sr, er do
			lines[#lines+1] = vim.api.nvim_buf_get_text(0, i - 1, sc, i - 1, ec + 1, {})[1]
		end
	end

	local ok, cmd = pcall(vim.fn.input, {
		prompt = '… | ',
		cancelreturn = vim.NIL,
	})

	if not ok or cmd == vim.NIL then
		return
	end

	local out = vim.fn.systemlist(cmd, lines)
	if mode == 'v' then
		vim.api.nvim_buf_set_text(0, sr - 1, sc, er - 1, ec + 1, out)
	elseif mode == 'V' then
		vim.api.nvim_buf_set_lines(0, sr - 1, er, true, out)
	elseif mode == '\x16' then
		for i = sr, er do
			vim.api.nvim_buf_set_text(0, i - 1, sc, i - 1, ec + 1, { out[i] })
		end
	end
end

vim.keymap.set('n', '<Plug>PipeNormal', ':set opfunc=v:lua.pipe_normal<CR>g@',
	{ silent = true })
vim.keymap.set('x', '<Plug>PipeVisual', ':<C-u>lua pipe_visual()<CR>',
	{ silent = true })

vim.keymap.set('n', '|', '<Plug>PipeNormal')
vim.keymap.set('x', '|', '<Plug>PipeVisual')
