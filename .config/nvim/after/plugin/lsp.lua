local conf = require('lspconfig')

conf.clangd.setup {}
conf.gopls.setup {}
conf.rust_analyzer.setup {}
conf.lua_ls.setup {
	settings = {
		Lua = {
			runtime = {
				version = 'LuaJIT',
			},
			diagnostics = {
				globals = {
					'vim',
					'require',
				},
			},
			workspace = {
				library = vim.api.nvim_get_runtime_file('', true),
			},
			telemetry = {
				enable = false,
			},
		},
	},
}

vim.api.nvim_create_autocmd('LspAttach', {
	group = vim.api.nvim_create_augroup('MangoLspConfig', {}),
	callback = function(ev)
		local function remap(mode, map, fn)
			vim.keymap.set(mode, map, fn, { buffer = ev.buf })
		end

		vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'
		remap('n', 'K', vim.lsp.buf.hover)
		remap('n', 'gd', vim.lsp.buf.definition)
		remap('n', 'gi', vim.lsp.buf.implementation)
		remap('n', 'gr', vim.lsp.buf.rename)
		remap('n', 'gt', vim.lsp.buf.type_definition)
		remap('n', 'g=', function() vim.lsp.buf.format { async = true } end)
		remap('n', ']d', vim.diagnostic.goto_prev)
		remap('n', '[d', vim.diagnostic.goto_next)
		remap('n', '<leader>la', vim.lsp.buf.code_action)
	end,
})
