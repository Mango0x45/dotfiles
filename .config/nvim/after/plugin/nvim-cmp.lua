local conf = require('lspconfig')
local capabilities = require('cmp_nvim_lsp').default_capabilities()
conf['clangd'].setup { capabilities = capabilities }
conf['gopls'].setup { capabilities = capabilities }
conf['lua_ls'].setup { capabilities = capabilities }
conf['rust_analyzer'].setup { capabilities = capabilities }

local cmp = require('cmp')
cmp.setup {
	snippet = {
		expand = function(args)
			require('luasnip').lsp_expand(args.body)
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
