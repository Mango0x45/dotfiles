local capabilities = require('cmp_nvim_lsp').default_capabilities()
local cmp = require('cmp')
local conf = require('lspconfig')
local snip = require('luasnip')

conf['clangd'].setup { capabilities = capabilities }
conf['gopls'].setup { capabilities = capabilities }
conf['lua_ls'].setup { capabilities = capabilities }
conf['rust_analyzer'].setup { capabilities = capabilities }
cmp.setup {
	snippet = {
		expand = function(args)
			snip.lsp_expand(args.body)
		end,
	},
	mapping = cmp.mapping.preset.insert({
		['<C-k>'] = cmp.mapping.select_prev_item(),
		['<C-j>'] = cmp.mapping.select_next_item(),
		['<C-e>'] = cmp.mapping.abort(),
		['<CR>'] = cmp.mapping.confirm({ select = true }),
	}),
	sources = cmp.config.sources({
		{ name = 'nvim_lsp' },
		{ name = 'path' },
		{ name = 'luasnip' },
	}, {
		{ name = 'buffer', length = 5 },
	}),
	experimental = {
		ghost_text = true,
	}
}
