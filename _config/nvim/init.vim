filetype plugin indent on
syntax enable

set encoding=utf-8
set fileencoding=utf-8

set shortmess+=I

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
set relativenumber
set numberwidth=5

set showmatch
set matchtime=2

set laststatus=2
set noshowmode

call plug#begin()
  " Colorscheme and status line.
  Plug 'morhetz/gruvbox'
  Plug 'itchyny/lightline.vim'
  " Syntax highlighting and indentation rules for multiple languages.
  Plug 'sheerun/vim-polyglot'
  " Add readline key bindings to where it makes sense.
  Plug 'tpope/vim-rsi'
  " Easily comment and surround stuff.
  Plug 'tpope/vim-surround'
  Plug 'tpope/vim-commentary'
  Plug 'tpope/vim-repeat'
  " Zig programming language support.
  Plug 'ziglang/zig.vim'
call plug#end()

set termguicolors
set background=light

colorscheme gruvbox
let g:lightline = { 'colorscheme': 'gruvbox' }

function! TrimTrailingWhitespace() abort
  if !&binary && &filetype != 'diff'
    let l:view = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:view)
  endif
endfunction

let mapleader=';'

noremap <leader>ws :call TrimTrailingWhitespace()<CR>
noremap <leader>cp :let @/ = ""<CR>
