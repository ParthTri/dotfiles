-- keybindings.lua

vim.g.mapleader = ' '

local keymap = vim.api.nvim_set_keymap
local opts = { noremap = true, silent = true }

keymap('n', '<C-s>', ':w<CR>', {})
keymap('n', '<Leader>v', '<c-v>', {})
keymap('i', 'jk', '<Esc>', {})

-- Windows
keymap('n', '<C-j>', '<C-w>j', {})
keymap('n', '<C-k>', '<C-w>k', {})
keymap('n', '<C-h>', '<C-w>h', {})
keymap('n', '<C-l>', '<C-w>l', {})

-- Buffers
keymap('n', '∂', ':bd<CR>', {})
keymap('n', '<C-n>', ':bnext<CR>', {})
keymap('n', '<C-p>', ':bprevious<CR>', {})

-- File Explorer
keymap('n', '<C-e>', ':Explore<CR>', {})
keymap('n', '<Leader>e', ':NnnExplorer<CR>', {})
keymap('n', '<Leader>E', ':NnnPicker<CR>', {})

-- LazyGit
keymap('n', '<Leader>g', ':LazyGit<CR>', {})

-- Comments
keymap('n', '<Leader>c', ':CommentToggle<CR>', {})
keymap('v', '<Leader>c', ':CommentToggle<CR>', {})

-- Telescope
keymap('n', '<Leader>ff', ':Telescope find_files hidden=true<CR>', {})
keymap('n', '<Leader>fb', ':Telescope buffers<CR>', {})
keymap('n', '<Leader>ft', ':TODOLocationList<CR>', {})

-- Saga
keymap('n', '<C-j>', '<Cmd>Lspsaga diagnostic_jump_next<CR>', opts)
keymap('n', 'K', '<Cmd>Lspsaga hover_doc<CR>', opts)
keymap('n', 'gd', '<Cmd>Lspsaga lsp_finder<CR>', opts)
keymap('i', '<C-k>', '<Cmd>Lspsaga signature_help<CR>', opts)
keymap('n', 'gp', '<Cmd>Lspsaga peek_definition<CR>', opts)
keymap('n', 'gr', '<Cmd>Lspsaga rename<CR>', opts)

-- Harpoon
keymap('n', 'ha', ':lua require("harpoon.mark").add_file()<CR>', {})
keymap('n', 'hf', ':lua require("harpoon.ui").toggle_quick_menu()<CR>', {})
keymap('n', 'hn', ':lua require("harpoon.ui").nav_next()<CR>', {})
keymap('n', 'hb', ':lua require("harpoon.ui").nav_prev()<CR>', {})

-- Outline
keymap('n', '<Leader>oo', ':SymbolsOutline<CR>', {})

-- Notes
keymap('n', "<leader>ww", ":VimwikiIndex<CR>", {})
keymap('n', '<Leader>wp', ":MarkdownPreview<CR>", {})
keymap('n', '<Leader>wP', ":MarkdownPreviewStop<CR>", {})

-- Zen
keymap('n', '<Leader>z', ':TZAtaraxis<CR>', {})
keymap('v', '<Leader>zn', ':TZNarrow<CR>', {})

-- Present
keymap('n', '<Leader>p', ':PresentingStart<CR>:TZAtaraxis<CR>', {})
keymap('n', '<Leader>P', ':TZAtaraxis<CR>:PresentingExit<CR>', {})

-- Terminal
keymap('n', '<Leader>oh', ':ToggleTerm direction=horizontal<CR>', {})
keymap('n', '<Leader>ov', ':ToggleTerm size=40 direction=vertical<CR>', {})
keymap('n', '<Leader>of', ':ToggleTerm direction=float<CR>', {})

