set nocompatible
set viminfo='20,\"50  "not store more than 50 registers in viminfo
if v:lang =~ "utf8$" || v:lang =~ "UTF-8$"
    set fileencodings=utf-8
endif

"set number
set history=100

if &term =~ "kterm"
    if has("terminfo")
        set t_Co=8
        set t_Sf=<Esc>[3%p1%dm
        set t_Sb=<Esc>[4%p1%dm
        set t_AB=<Esc>[%?%p1%{8}%<%t25;%p1%{40}%+%e5;%p1%{32}%+%;%dm
        set t_AF=<Esc>[%?%p1%{8}%<%t22;%p1%{30}%+%e1;%p1%{22}%+%;%dm
    else
        set t_Co=8
        set t_Sf=<Esc>[3%dm
        set t_Sb=<Esc>[4%dm
    endif
endif

"set t_Co=8
"set background=light
"colorscheme peachpuff
"colorscheme desert256
"colorscheme inkpot

if &t_Co > 2 || has("gui_running")
    syntax on
    set hlsearch
endif

syntax enable
filetype on
filetype plugin on
filetype indent on

set list
set magic
set listchars=tab:\|\ ",eol:$
" set mouse=a
set mousemodel=extend
set incsearch
set ignorecase
set showmatch

set autoindent
set smartindent

set tabstop=4
set softtabstop=4
set shiftwidth=4
"set smarttab
set expandtab

set backspace=2
set backspace=indent,eol,start

"lang mes en

set noerrorbells

set laststatus=2
set statusline=[OS=%{&ff}]\ [TYPE=%Y]\ [%p%%]%=[POS=%l,%v]\ [%f]

"highlight CurrentLine guibg=gray
set showcmd
" Fold
" set nofen
" set foldenable
" set foldmethod=indent
"set foldcolumn=2

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
let g:explVertical=1	    " split horizontally
let g:explSplitRight=1		" put the explorer window left
let g:explStartRight=0		" put the explorer window left
let g:explWinSize=30
let g:explSortBy='name'
let g:explDirsFirst=1
"let g:explVertical=1		" split vertically
"et g:explSplitRight=0		" put the explorer window left
map ,se :Sexplore<CR>
map ,exp :Explore<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""
"          Auto Change Date
" autodate.vim: include "Last Change: ."
"""""""""""""""""""""""""""""""""""""""""""""""""""""""
let autodate_format = '%Y/%m/%d-%H:%M:%S '


"""""""""""""""" VTreeExplore"""""""""""""""""""""
let treeExplVertical=1
map :te :VSTreeExplore<CR>
map ,te :VSTreeExplore<CR>

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

"nmap \sh    :source ~/.vim/plugin/vimsh.vim<CR>
"let g:vimsh_prompt_override = 1 " will not use normal prompt from your 'real' shell
"let g:vimsh_prompt_pty      = "%m%#" "  specify overriden prompt
"let g:vimsh_split_open      = 0     "  run vimsh in the current buffer
"let g:vimsh_sh              = "/bin/tcsh" "     shell to run within vimsh
"let g:vimsh_pty_prompt_override = 0
"let $VIMSH                      = 1

map     <F4>    :q<CR>
imap    <F4>    <ESC>:q<CR>

map     <F12>   a<c-r>=strftime("%Y-%m-%d %H:%H:%S")<CR><CR>
imap    <F12>    <c-r>=strftime("%Y-%m-%d %H:%H:%S")<CR><CR>

" set file type to verilog
map ,fv :set ft=verilog<CR>

set guifont=Bitstream_Vera_Sans_Mono:h11:cANSI
