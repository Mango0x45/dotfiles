local lsp = require('lsp-zero')
local lib = require('mango.lib')

lsp.preset('recommended')
lsp.ensure_installed({
	'clangd',
	'gopls',
	'rust_analyzer',
})

lsp.nvim_workspace()

local cmp = require('cmp')
local cmp_select = { behavior = cmp.SelectBehavior.Select }
local cmp_mappings = lsp.defaults.cmp_mappings({
	['<C-k>'] = cmp.mapping.select_prev_item(cmp_select),
	['<C-j>'] = cmp.mapping.select_next_item(cmp_select),
	['<CR>'] = cmp.mapping.confirm({ select = true }),
	['<C-Space>'] = cmp.mapping.complete(),
})

lsp.set_preferences({
	suggest_lsp_servers = false,
	sign_icons = {}
})

lsp.setup_nvim_cmp({
	mapping = cmp_mappings
})

lsp.on_attach(function(_, bufnr)
	local opts = { buffer = bufnr, remap = false }

	lib.remap('n', 'gd', vim.lsp.buf.definition, opts)
	lib.remap('n', '<leader>la', vim.lsp.buf.code_action, opts)
	lib.remap('n', '<leader>lr', vim.lsp.buf.rename, opts)
	lib.remap('n', '<leader>l=', vim.lsp.buf.format, opts)
end)

lsp.setup()
