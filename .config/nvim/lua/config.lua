-- config.lua

vim.opt.termguicolors = true
vim.bo.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.opt.tabstop = 2

vim.wo.number = true
vim.opt.relativenumber = true
vim.api.nvim_set_option("clipboard", "unnamed")

-- Base 16 Color scheme
vim.cmd("colorscheme base16-kanagawa")
vim.cmd("highlight Normal guibg=none ctermbg=none") -- Normal focused
vim.cmd("highlight EndOfBuffer guibg=none ctermbg=none") -- Extra space at the end of buffer
vim.cmd("highlight SignColumn guibg=none ctermbg=none") -- Extra sign column before numbers
vim.cmd("highlight LineNr guibg=none ctermbg=none") -- Line numbers
vim.cmd("highlight NormalNC guibg=none ctermbg=none") -- Other windows

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
    ['<TAB>'] = cmp.mapping.confirm({ select = true }),
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
require('staline').setup()

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

