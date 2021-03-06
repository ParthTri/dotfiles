-- packages.lua
require('packer').startup(function()
  -- Packages
  use 'wbthomason/packer.nvim'

  -- theme
  use "rebelot/kanagawa.nvim" 

  -- programming 
  use 'ray-x/go.nvim'
  use 'ray-x/guihua.lua'

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

	-- Git
	use { 'TimUntersberger/neogit', requires = 'nvim-lua/plenary.nvim' }

	-- Status line
	use 'tamton-aquib/staline.nvim'

	-- Tab Bar
	use {
		'romgrk/barbar.nvim',
		requires = {'kyazdani42/nvim-web-devicons'}
	}
end)
