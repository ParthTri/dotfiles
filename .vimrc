set nocompatible
filetype off
syntax on
set ruler
set textwidth=0
set number
set encoding=utf-8
set backspace=indent,eol,start
set softtabstop=2
set tabstop=4

call plug#begin('~/.vim/plugged')

Plug 'vim-airline/vim-airline'
Plug 'vim-scripts/indentpython.vim'
Plug 'nvie/vim-flake8'
Plug 'joshdick/onedark.vim'

call plug#end()

colorscheme onedark

let g:rehash256 = 1
" let g:airline_theme = onedark
set termguicolors

set splitbelow
set splitright
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

au BufNewFile, BufRead *.py
  \| set shiftwidth=2
  \| set textwidth=79
  \| set expandtab
  \| set autoindent
" File manager
nmap <F6> :Lexplore<CR>
let g:netrw_liststyle = 3
let g:netrw_banner = 0
let g:netrw_browse_split = 0
let g:netrw_winsize = 25


highlight Normal ctermbg=none
highlight NonText ctermbg=none

let g:kite_supported_languages = ['*']
let g:kite_previous_placeholder = '<C-H>'
let g:kite_next_placeholder = '<C-L>'
set statusline=%<%f\ %h%m%r%{kite#statusline()}%=%-14.(%l,%c%V%)\ %P
set laststatus=2
