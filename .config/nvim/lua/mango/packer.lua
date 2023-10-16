vim.cmd.packadd('packer.nvim')

return require('packer').startup(function(use)
	use 'wbthomason/packer.nvim'

	use 'wellle/targets.vim'

	use {
		'nvim-telescope/telescope.nvim',
		tag = '0.1.2',
		requires = { { 'nvim-lua/plenary.nvim' } }
	}

	use {
		'rose-pine/neovim',
		as = 'rose-pine',
		config = function()
			vim.cmd.colorscheme('rose-pine')
		end
	}

	use {
		'nvim-treesitter/nvim-treesitter-textobjects',
		after = 'nvim-treesitter',
		requires = 'nvim-treesitter/nvim-treesitter',
		run = ':TSUpdate',
	}

	use 'mbbill/undotree'
	use 'tpope/vim-fugitive'
	use 'tpope/vim-surround'
	use 'tpope/vim-commentary'
	use 'tpope/vim-speeddating'
	use 'tpope/vim-vinegar'
	use 'mattn/emmet-vim'

	use {
		'VonHeikemen/lsp-zero.nvim',
		branch = 'v2.x',
		requires = {
			-- LSP Support
			{ 'neovim/nvim-lspconfig' },
			{ 'williamboman/mason.nvim' },
			{ 'williamboman/mason-lspconfig.nvim' },

			-- Autocompletion
			{ 'hrsh7th/nvim-cmp' },
			{ 'hrsh7th/cmp-nvim-lsp' },
			{ 'L3MON4D3/LuaSnip' },
		}
	}
end)
