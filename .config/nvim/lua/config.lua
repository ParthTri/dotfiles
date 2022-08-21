-- config.lua

vim.opt.termguicolors = true
vim.bo.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.opt.tabstop = 2

vim.wo.number = true
vim.opt.relativenumber = true
vim.api.nvim_set_option("clipboard", "unnamed")

-- Tokyo Night Colorscheme
vim.g.tokyonight_style = "night"
vim.g.tokyonight_italic_functions = true
vim.g.tokyonight_transparent = true
vim.g.tokyonight_transparent_sidebar = true
vim.g.tokyonight_hide_inactive_statusline = true
vim.g.tokyonight_sidebars = { "qf", "vista_kind", "terminal", "packer" }
vim.cmd[[colorscheme tokyonight]]

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
  }),
	source_mapping
})

-- Treesitter 
local ts = require("nvim-treesitter")
ts.setup({
  highlight = {
    enable = true,
  },
  indent = {
    enable = true,
  },
  ensure_installed = {
		"typescript",
    "tsx",
		"javascript",
    "toml",
    "fish",
    "json",
    "yaml",
    "css",
    "html",
    "lua",
		"python",
  },
  autotag = {
    enable = true,
  },
})

local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.tsx.filetype_to_parsername = { "javascript", "typescript.ts"}

-- LSP Saga
local saga = require('lspsaga')
saga.init_lsp_saga()

vim.cmd("highlight LspFloatWinNormal guibg=none ctermbg=none")
vim.cmd("highlight LspSagaFinderSelection guibg=none")

-- Snippets
require("luasnip.loaders.from_vscode").lazy_load()

-- neogit
local neogit = require('neogit')
neogit.setup {}

-- Status line
require('lualine').setup({
  options = {
    theme = "tokyonight"
  }
})

-- Formatting
local null_ls= require("null-ls")
local sources = {
	null_ls.builtins.formatting.prettier.with({
			filetypes = { "html", "css", "javascript", "json", "yaml", "markdown" },
	}),
  null_ls.builtins.formatting.prettier,
	null_ls.builtins.completion.spell
}

null_ls.setup({ sources = sources })

-- Todo Comments
require('todo').setup({
  keyword = {
    FIX = { icon = " ", color = "error", alt = { "FIXME", "BUG", "FIXIT", "ISSUE" }},
    TODO = { icon = " ", color = "info" },
    WARN = { icon = " ", color = "warning", alt = { "WARNING" } },
    NOTE = { icon = " ", color = "hint", alt = { "INFO" } }
  },
  colors = {
    error = { "DiagnosticError", "ErrorMsg", "#E82424" },
    warning = { "DiagnosticWarn", "WarningMsg", "#FF9E3B" },
    info = { "DiagnosticInfo", "#7FB4CA" },
    hint = { "DiagnosticHint", "#76946A" },
    default = { "Identifier", "#7C3AED" }
  }
})

