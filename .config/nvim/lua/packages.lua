-- packages.lua
require('packer').startup(function() -- Packages 
	use 'wbthomason/packer.nvim'

	-- theme
	use "rebelot/kanagawa.nvim"

	-- programming 
	use 'ray-x/go.nvim'

	-- File Explorer
	use 'kyazdani42/nvim-web-devicons'
	use 'kyazdani42/nvim-tree.lua'

	-- Telescope
	use {
		'nvim-telescope/telescope.nvim',
		requires = { {'nvim-lua/plenary.nvim'} }
	}

	-- Comment
	use "terrortylor/nvim-comment"

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
	use "glepnir/lspsaga.nvim"

	-- Git
	use 'kdheepak/lazygit.nvim'
	use 'lewis6991/gitsigns.nvim'

	-- Status line
	use 'nvim-lualine/lualine.nvim'

	-- Automatic Tag Renaming
	use "AndrewRadev/tagalong.vim"

	-- Formatting
	use "jose-elias-alvarez/null-ls.nvim"

	-- Todo Comments
	use {
		"AmeerTaweel/todo.nvim",
		requires = "nvim-lua/plenary.nvim"
	}

	-- Harpoon
	use "ThePrimeagen/harpoon"

	-- Code Outline
	use 'stevearc/aerial.nvim'

	-- Note taking
	use 'vimwiki/vimwiki'
	use({
			"iamcco/markdown-preview.nvim",
			run = function() vim.fn["mkdp#util#install"]() end,
	})
	use 'Pocco81/true-zen.nvim'
	use 'folke/twilight.nvim'

	-- Presenter
	use "sotte/presenting.vim"
end)
