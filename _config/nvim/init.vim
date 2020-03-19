filetype plugin indent on
syntax enable

set encoding=utf-8
set fileencoding=utf-8

set nobackup
set noswapfile
set lazyredraw

set autoindent
set smarttab
set backspace=indent,eol,start

set tabstop=8
set softtabstop=2
set shiftwidth=2
set expandtab

set hlsearch
set incsearch
set ignorecase
set smartcase
set inccommand=nosplit

set sidescrolloff=5
set scrolloff=8

set number
set numberwidth=5

set showmatch
set matchtime=2

set laststatus=2
set noshowmode

call plug#begin()
  Plug 'morhetz/gruvbox'
  Plug 'itchyny/lightline.vim'
  Plug 'tpope/vim-surround'
  Plug 'tpope/vim-repeat'
call plug#end()

colorscheme gruvbox
let g:lightline = { 'colorscheme': 'seoul256' }

function! TrimTrailingWhitespace() abort
  let l:view = winsaveview()
  keeppatterns %s/\s\+$//e
  call winrestview(l:view)
endfunction

let mapleader = ","

noremap <leader>ws :call TrimTrailingWhitespace()<CR>
