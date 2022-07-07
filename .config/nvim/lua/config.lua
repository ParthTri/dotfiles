-- config.lua

vim.bo.expandtab = true
vim.bo.shiftwidth = 2
vim.bo.softtabstop = 2
vim.wo.number = true
vim.opt.relativenumber = true

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
require("nvim-tree").setup()

-- lsp
local lsp = require('lsp-zero')

lsp.preset('recommended')
lsp.nvim_workspace()
lsp.setup()

-- neogit
local neogit = require('neogit')

neogit.setup {}
-- Status line_lenses
require('hardline').setup()
