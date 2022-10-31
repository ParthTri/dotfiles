-- config.lua

vim.opt.termguicolors = true
vim.bo.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.opt.tabstop = 2

vim.wo.number = true
vim.opt.relativenumber = true
vim.api.nvim_set_option("clipboard", "unnamed")


-- Kanagawa Colorscheme
require("kanagawa").setup({
  undercurl = true,           -- enable undercurls
  commentStyle = { italic = true },
  functionStyle = { italic=true},
  keywordStyle = { italic = true},
  statementStyle = { bold = true },
  typeStyle = {},
  variablebuiltinStyle = { italic = true},
  specialReturn = true,
  specialException = true,
  transparent = true,
  dimInactive = false,
  globalStatus = false,
  terminalColors = true,
  theme = "default",
  overrides = {
    Folded = { bg = "none"}
  }
})
vim.cmd[[colorscheme kanagawa]]

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

-- Go support
require('go').setup()

-- lsp
local lsp = require('lsp-zero')

lsp.preset('recommended')
lsp.nvim_workspace()
lsp.setup()

require('lspconfig').emmet_ls.setup{
	filetypes = {"html", "svelte", "jsx", "tsx", "css", "scss"}
}

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
require("nvim-treesitter.configs").setup({
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
    "go"
  },
  autotag = {
    enable = true,
  },
})

local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.tsx.filetype_to_parsername = { "javascript", "typescript.ts"}

require('treesitter-context').setup()

-- Tressitter Code folding
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
vim.opt.foldlevel = 20
vim.opt.foldtext = "v:lua.custom_fold_text()"
vim.opt.fillchars = { eob = "-", fold = " " }
vim.opt.viewoptions:remove("options")

function _G.custom_fold_text()
    local line = vim.fn.getline(vim.v.foldstart)
    local line_count = vim.v.foldend - vim.v.foldstart + 1
		local line_end = vim.fn.getline(vim.v.foldend)
    line_end = string.gsub(line_end, "%s", "")

    return line .. " " .. line_count .. " lines ".. line_end
end

-- LSP Saga
local saga = require('lspsaga')
saga.init_lsp_saga()

vim.cmd("highlight LspFloatWinNormal guibg=none ctermbg=none")
vim.cmd("highlight LspSagaFinderSelection guibg=none")

-- Snippets
require("luasnip.loaders.from_vscode").lazy_load()

-- Status line
require('lualine').setup()

-- Git
require("gitsigns").setup()

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

-- Code Outline
require('aerial').setup({})

-- Zen mode
require('true-zen').setup({
  integrations = {
    twilight = true,
    lualine = true
  }
})

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

