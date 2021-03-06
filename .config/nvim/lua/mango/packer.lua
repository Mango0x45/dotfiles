vim.cmd.packadd('packer.nvim')

return require('packer').startup(function(use)
	-- Package manager
	use 'wbthomason/packer.nvim'

	-- Colorscheme
	use {
		'rose-pine/neovim',
		as = 'rose-pine',
	}

	-- More textobjects; more is more
	use 'wellle/targets.vim'
	use {
		'nvim-treesitter/nvim-treesitter-textobjects',
		after = 'nvim-treesitter',
		requires = 'nvim-treesitter/nvim-treesitter',
		run = ':TSUpdate',
	}

	-- Efficiency improvements
	use 'kylechui/nvim-surround'
	use 'mattn/emmet-vim'
	use 'numToStr/Comment.nvim'
	use 'christoomey/vim-sort-motion'

	-- Git integration
	use 'tpope/vim-fugitive'

	-- Improvements to builtin functionality
	use 'mbbill/undotree'
	use 'tpope/vim-speeddating'

	-- Additional language support
	use 'Glench/Vim-Jinja2-Syntax'
	use 'https://git.sr.ht/~mango/tree-sitter-gsp'
	use 'luckasRanarison/tree-sitter-hypr'

	-- LSP support and completions
	use 'neovim/nvim-lspconfig'
	use {
		'hrsh7th/nvim-cmp',
		requires = {
			-- Completion backends
			{ 'hrsh7th/cmp-buffer' },
			{ 'hrsh7th/cmp-nvim-lua' },
			{ 'hrsh7th/cmp-nvim-lsp' },
			{ 'hrsh7th/cmp-path' },

			-- Snippets
			{ 'L3MON4D3/LuaSnip' },
			{ 'saadparwaiz1/cmp_luasnip' },
		},
	}

	-- Function context
    use 'nvim-treesitter/nvim-treesitter-context'
end)
