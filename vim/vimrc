set number

set nocompatible

set history=100

syntax on
syntax enable
filetype on
filetype plugin on
filetype indent on

set list
set magic
set listchars=tab:\|\ ,eol:$
set mouse=a
set hlsearch
set incsearch
set ignorecase
set showmatch

set autoindent
set smartindent

set tabstop=4
set softtabstop=4
set shiftwidth=4
set smarttab

set backspace=2
set backspace=indent,eol,start

"lang mes en

set noerrorbells

" Fold
" set nofen
" set foldenable
" set foldmethod=indent

""""""""""""TagList"""""""""""""""""
let Tlist_Auto_Update=1
let Tlist_Use_Right_Window=1
let Tlist_Sort_Type="name"
let Tlist_Exit_OnlyWindow=1
let Tlist_Show_One_File=1

map to :TlistOpen<CR>
map tc :TlistClose<CR>
map tt :TlistToggle<CR>

"""""""""""""""Explore"""""""""""""""
let g:explSplitVertical=0	" split horizontally
let g:explSplitRight=0		" put the explorer window left
let g:explStartRight=0		" put the explorer window left
let g:explWinSize=30
let g:explSortBy='name'
let g:explDirsFirst=1
"let g:explVertical=1		" split vertically
"et g:explSplitRight=0		" put the explorer window left
map :se :Sexplore<CR>
map :exp :Explore<CR>

"""""""""""""""""""""""""""""""""""""
"Close Pair
"""""""""""""""""""""""""""""""""""""
:inoremap ( ()<ESC>i
:inoremap ) <c-r>=ClosePair(')')<CR>
:inoremap { {}<ESC>i
:inoremap } <c-r>=ClosePair('}')<CR>
:inoremap [ []<ESC>i
:inoremap ] <c-r>=ClosePair(']')<CR>
":inoremap < <><ESC>i
":inoremap > <c-r>=ClosePair('>')<CR>

function ClosePair(char)
	if getline('.')[col('.') - 1] == a:char
		return "\<Right>"
	else
		return a:char
	endif
endf

