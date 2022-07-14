-- keybindings.lua

vim.g.mapleader = ' '

local keymap = vim.api.nvim_set_keymap
keymap('n', '<C-s>', ':w<CR>', {})

-- Windows
keymap('n', '<C-j>', '<C-w>j', {})
keymap('n', '<C-k>', '<C-w>k', {})
keymap('n', '<C-h>', '<C-w>h', {})
keymap('n', '<C-l>', '<C-w>l', {})

-- Buffers
keymap('n', '<C-d>', ':bd<CR>', {})

-- File Explorer
keymap('n', '<C-e>', ':NvimTreeToggle<CR>', {})

-- Neogit
keymap('n', '<Leader>g', ':Neogit<CR>', {})

-- Comments
keymap('n', '<C-c>', ':CommentToggle<CR>', {})
keymap('v', '<C-c>', ':CommentToggle<CR>', {})

-- Tabs
keymap('n', 'L', ':BufferNext<CR>', {})
keymap('n', 'H', ':BufferPrevious<CR>', {})

-- Telescope
keymap('n', '<Leader>ff', ':Telescope find_files<CR>', {})
keymap('n', '<Leader>fb', ':Telescope buffers<CR>', {})
