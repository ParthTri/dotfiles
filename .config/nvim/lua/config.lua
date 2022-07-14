-- config.lua

vim.bo.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.opt.tabstop = 2

vim.wo.number = true
vim.opt.relativenumber = true
vim.api.nvim_set_option("clipboard","unnamed")

-- colorscheme
require("kanagawa").setup({
	undercurl = true,           -- enable undercurls
	commentStyle = { italic = true },
	functionStyle = {},
	keywordStyle = { italic = true},
	statementStyle = { bold = true },
	typeStyle = {},
	variablebuiltinStyle = { italic = true},
	specialReturn = true,       -- special highlight for the return keyword
	specialException = true,    -- special highlight for exception handling keywords
	transparent = true,        -- do not set background color
	dimInactive = false,        -- dim inactive window `:h hl-NormalNC`
	globalStatus = false,       -- adjust window separators highlight for laststatus=3
	colors = {},
	overrides = {},
})
vim.cmd("colorscheme kanagawa")

-- file explorer
require("nvim-tree").setup({
	filters = {
		dotfiles = false,
	}
})

-- Telescope
require("telescope").setup()

-- Comment
require('nvim_comment').setup()

-- lsp
local lsp = require('lsp-zero')

lsp.preset('recommended')
lsp.nvim_workspace()
lsp.setup()

local cmp = require("cmp")
local source_mapping = {
	buffer = "[Buffer]",
	nvim_lsp = "[LSP]",
	nvim_lua = "[Lua]",
	cmp_tabnine = "[TN]",
	path = "[Path]",
}

cmp.setup({
	mapping = cmp.mapping.preset.insert({
		['<C-y>'] = cmp.mapping.confirm({ select = true }),
			["<C-u>"] = cmp.mapping.scroll_docs(-4),
			["<C-d>"] = cmp.mapping.scroll_docs(4),
			["<C-Space>"] = cmp.mapping.complete(),
		})
})

-- Snippets
require("luasnip.loaders.from_vscode").lazy_load()

-- neogit
local neogit = require('neogit')
neogit.setup {}

-- Status line
require('hardline').setup()

-- Tab Bar
require('bufferline').setup({
	animation = true,
	auto_hide = false,
	tab_pages = true,
	closable = true,
	clickable = true,
	icons = true,
	insert_at_end = true,
	insert_at_start = false
})

