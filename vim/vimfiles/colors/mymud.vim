" mymud.vim.vim: a new colorscheme by vincent
" mix mud and desert colorscheme to get a better one for me 
"

set background=light
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name="mud"

hi Normal   guifg=#ffffcc	 guibg=#330000 
hi LineNr   guifg=white guibg=#330000 
hi Statusline    gui=none guibg=#993300 guifg=#ffffff
hi StatuslineNC    gui=none guibg=#660000 guifg=#ffffff
hi VertSplit    gui=none guibg=#330000 guifg=#ffffff
hi Cursor    gui=none guibg=DodgerBlue guifg=#ffffff
hi CursorLine gui=none guibg=#000000 term=underline cterm=underline
hi CursorColumn gui=none guibg=#000000 term=underline cterm=underline

hi Title    guifg=black	 guibg=white gui=BOLD
hi lCursor  guibg=Cyan   guifg=NONE

" syntax highlighting groups
hi Comment    gui=NONE guifg=#996666
hi Operator   guifg=#ff0000

hi Identifier    guifg=#33ff99 gui=NONE

hi Statement	 guifg=#cc9966 gui=NONE
hi TypeDef       guifg=#c000c8 gui=NONE
hi Type          guifg=#ccffff gui=NONE
hi Boolean       guifg=#ff00aa gui=NONE

hi String        guifg=#99ccff gui=NONE
hi Number        guifg=#66ff66 gui=NONE
hi Constant      guifg=#f0f000 gui=NONE

hi Function      gui=NONE      guifg=#fffcfc 
hi PreProc       guifg=#cc6600 gui=NONE
hi Define        gui=bold guifg=#f0f0f0 
hi Special       gui=none guifg=#cccccc 
hi BrowseDirectory  gui=none guifg=#FFFF00 

hi Keyword       guifg=#ff8088 gui=NONE
hi Search        gui=NONE guibg=#ffff00 guifg=#330000 
hi IncSearch     gui=NONE guifg=#fcfcfc guibg=#8888ff
hi SpecialKey    gui=NONE guifg=#fcfcfc guibg=#8888ff
hi NonText       gui=NONE guifg=#fcfcfc 
hi Directory     gui=NONE guifg=#999900

hi Visual	gui=none guifg=khaki guibg=olivedrab
hi Folded	guibg=grey30 guifg=gold 
hi FoldColumn	guibg=grey30 guifg=tan

" for terminal 
hi DiffAdd       ctermbg=4 gui=bold,italic guifg=#000000 guibg=#A86828
hi DiffChange    ctermbg=5  gui=bold,italic guifg=#000000 guibg=#986888 
hi DiffDelete    cterm=bold ctermfg=4 ctermbg=6 gui=bold guifg=#0000ff guibg=#68A898
hi DiffText      cterm=bold ctermbg=1 gui=bold,italic guifg=#000000 guibg=#980000

" for omni completion
hi Pmenu       guibg=grey30
hi PmenuSel    guifg=DarkBlue guibg=LightGreen
hi PmenuSbar   guibg=Black 
hi PmenuThumb  guifg=SeaGreen	

