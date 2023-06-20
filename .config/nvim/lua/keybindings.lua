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
keymap('n', '<Leader>bd', ':bd<CR>', {})
keymap('n', '<Leader>bn', ':bnext<CR>', {})
keymap('n', '<Leader>bp', ':bprevious<CR>', {})
keymap('n', '<Leader>bb', ':buffers<CR>', {})

-- File Explorer
-- function get_project_root()
-- 	local curr_path = vim.api.nvim_exec('pwd', true)
-- 	return curr_path
-- end
--
-- keymap('n', '<Leader>e', ':Explore<CR>', {})
-- keymap('n', '<C-e>', string.format(':Vexplore %s<CR>', get_project_root()), {})

keymap('n', '<C-e>', ':NvimTreeToggle<CR>', {})

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

-- Outline
keymap('n', '<Leader>oo', ':SymbolsOutline<CR>', {})

-- Terminal
keymap('n', '<Leader>oh', ':ToggleTerm direction=horizontal<CR>', {})
keymap('n', '<Leader>ov', ':ToggleTerm size=40 direction=vertical<CR>', {})
keymap('n', '<Leader>of', ':ToggleTerm direction=float<CR>', {})

