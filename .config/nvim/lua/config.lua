
vim.opt.termguicolors = true
vim.bo.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.opt.tabstop = 2

vim.wo.number = true
vim.opt.relativenumber = true
vim.opt.clipboard = 'unnamedplus'

-- Line Wrapping
vim.wo.wrap = true
vim.wo.linebreak = true
vim.wo.breakindent = true

-- Minimal colorscheme
vim.g.gruvbox_material_background = 'hard'
vim.g.gruvbox_material_foreground = 'material'
vim.g.gruvbox_material_better_performance = 1
vim.g.gruvbox_material_enable_bold = 1
vim.g.gruvbox_material_enable_italic = 1

vim.cmd[[ colorscheme  gruvbox-material ]]

-- Netrw file explorer
vim.g.netrw_liststyle = 3
vim.g.netrw_banner = 0
vim.g.netrw_hidden = 1
vim.g.netrw_localcopydircmd = 'cp -r'
vim.g.netrw_winsize = 30

-- Telescope
require("telescope").setup({
	ignore_file_patterns = {
		"/.git/",
		"node_modules/",
		"__pycache__",
		".venv"
	}	
})

-- Comment
-- require('nvim_comment').setup()
require('Comment').setup()

-- Go support
require('go').setup({})

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
  manage_nvim_cmp = false,
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
   "go",
   "markdown_inline"
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
saga.setup({})

vim.cmd("highlight LspFloatWinNormal guibg=none ctermbg=none")
vim.cmd("highlight LspSagaFinderSelection guibg=none")

-- Snippets
require("luasnip.loaders.from_vscode").lazy_load()

-- Status line
require('lualine').setup({
  theme = 'gruvbox-material'
})

-- Git
require("gitsigns").setup()

-- Highlight colour git signs
vim.cmd("highlight GitSignsAdd guibg=NONE")
vim.cmd("highlight GitSignsChange guibg=NONE")
vim.cmd("highlight GitSignsDelete guibg=NONE")

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
require('symbols-outline').setup({
  width = 15
})

-- Terminal
require('toggleterm').setup({
  open_mapping = [[<c-\>]],
  persist = true
})

-- Which Key
vim.o.timeout = true
vim.o.timeoutlen = 200
require('which-key').setup({
})

-- Nvim Tree
require("nvim-tree").setup()
