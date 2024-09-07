-- packages.lua
require('packer').startup(function() -- Packages 
	use 'wbthomason/packer.nvim'

	-- theme
	use "sainnhe/gruvbox-material"
	use "rebelot/kanagawa.nvim"

	-- programming 
	use 'ray-x/go.nvim'
	use 'alaviss/nim.nvim'

	-- Telescope
	use {
		'nvim-telescope/telescope.nvim',
		requires = { {'nvim-lua/plenary.nvim'} }
	}

	-- Comment
	use "numToStr/Comment.nvim"

	-- Tree Sitter
	use "nvim-treesitter/nvim-treesitter"
	use 'nvim-treesitter/nvim-treesitter-context'

	-- LSP
	use {
		'VonHeikemen/lsp-zero.nvim',
		requires = {
			-- LSP Support
			{'neovim/nvim-lspconfig'},
			{'williamboman/nvim-lsp-installer'},

			-- Autocompletion
			{'hrsh7th/nvim-cmp'},
			{'hrsh7th/cmp-buffer'},
			{'hrsh7th/cmp-path'},
			{'saadparwaiz1/cmp_luasnip'},
			{'hrsh7th/cmp-nvim-lsp'},
			{'hrsh7th/cmp-nvim-lua'},

			-- Snippets
			{'L3MON4D3/LuaSnip'},
			{'rafamadriz/friendly-snippets'},
		}
	}

	-- LSP Saga
	use { 
		"nvimdev/lspsaga.nvim",
		requires = {
			{'nvim-tree/nvim-web-devicons'},
		}
	}

	-- Git
	use 'kdheepak/lazygit.nvim'
	-- use 'aspeddro/gitui.nvim'
	use 'lewis6991/gitsigns.nvim'

	-- Status line
	use 'nvim-lualine/lualine.nvim'

	-- Automatic Tag Renaming
	use "AndrewRadev/tagalong.vim"

	-- Formatting
	use "jose-elias-alvarez/null-ls.nvim"

	-- Code Outline
	use 'simrat39/symbols-outline.nvim'

	-- Ledger
	use 'ledger/vim-ledger'

	-- Terminal
	use 'akinsho/toggleterm.nvim'

	-- Which key
	use 'folke/which-key.nvim'

	-- Folding Pairs
	use "jiangmiao/auto-pairs"

	-- Org mode
	use "nvim-orgmode/orgmode"

	-- Tree
	use "nvim-tree/nvim-tree.lua"

	-- Prettier
	use "MunifTanjim/prettier.nvim"

end)
