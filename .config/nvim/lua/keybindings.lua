-- keybindings.lua

local keymap = vim.api.nvim_set_keymap
keymap('n', '<C-s>', ':w<CR>', {})

-- Windows
keymap('n', '<C-j>', '<C-w>j', {})
keymap('n', '<C-k>', '<C-w>k', {})
keymap('n', '<C-h>', '<C-w>h', {})
keymap('n', '<C-l>', '<C-w>l', {})

-- Buffers
keymap('n', 'L', ':bn<CR>', {})
keymap('n', 'H', ':bp<CR>', {})
keymap('n', '<C-d>', ':bd<CR>', {})

-- File Explorer
keymap('n', '<C-e>', ':NvimTreeToggle<CR>', {})

-- Neogit
keymap('n', '<C-g>', ':Neogit<CR>', {})

-- Comments
keymap('n', '<C-c>', ':CommentToggle<CR>', {})
