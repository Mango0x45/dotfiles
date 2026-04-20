set background=light
highlight clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "mango-light"

" Normal & Background
hi Normal         guifg=#383A42 ctermfg=237 guibg=#F4F5F7 ctermbg=255
hi EndOfBuffer    guifg=#E8EAEF ctermfg=254 guibg=#F4F5F7 ctermbg=255
hi SignColumn     guibg=#F4F5F7 ctermbg=255

" Cursor & Lines
hi Cursor         guifg=#F4F5F7 ctermfg=255 guibg=#383A42 ctermbg=237
hi CursorLine     guibg=#E1E4EA ctermbg=252 term=NONE cterm=NONE gui=NONE
hi ColorColumn    guibg=#E1E4EA ctermbg=252
hi LineNr         guifg=#696C77 ctermfg=243 guibg=#F4F5F7 ctermbg=255
hi CursorLineNr   guifg=#B85C19 ctermfg=130 guibg=#F4F5F7 ctermbg=255 term=bold cterm=bold gui=bold

" Selection
hi Visual         guibg=#E1E4EA ctermbg=252

" Window & Status
hi VertSplit      guifg=#E8EAEF ctermfg=254 guibg=#E8EAEF ctermbg=254
hi StatusLine     guifg=#383A42 ctermfg=237 guibg=#E1E4EA ctermbg=252 gui=bold cterm=bold
hi StatusLineNC   guifg=#696C77 ctermfg=243 guibg=#E8EAEF ctermbg=254 gui=NONE cterm=NONE
hi TabLine        guifg=#696C77 ctermfg=243 guibg=#E8EAEF ctermbg=254 gui=NONE cterm=NONE
hi TabLineFill    guibg=#E8EAEF ctermbg=254
hi TabLineSel     guifg=#383A42 ctermfg=237 guibg=#F4F5F7 ctermbg=255 gui=bold cterm=bold

" Syntax
hi Comment        guifg=#696C77 ctermfg=243
hi Constant       guifg=#B85C19 ctermfg=130
hi String         guifg=#B85C19 ctermfg=130
hi Number         guifg=#B85C19 ctermfg=130
hi Identifier     guifg=#287A99 ctermfg=31  gui=NONE cterm=NONE
hi Function       guifg=#986800 ctermfg=136
hi Statement      guifg=#8F38B8 ctermfg=90  gui=bold cterm=bold
hi Keyword        guifg=#8F38B8 ctermfg=90  gui=bold cterm=bold
hi PreProc        guifg=#A838A8 ctermfg=127 gui=bold cterm=bold
hi Type           guifg=#2E68B8 ctermfg=25  gui=NONE cterm=NONE
hi Special        guifg=#D03E3E ctermfg=160
hi Error          guifg=#D03E3E ctermfg=160 guibg=NONE  gui=bold,reverse cterm=bold,reverse
hi Todo           guifg=#A838A8 ctermfg=127 guibg=NONE  gui=bold cterm=bold
hi MatchParen     guifg=#52A628 ctermfg=70  guibg=#E1E4EA ctermbg=252 gui=bold cterm=bold

" Diffs
hi DiffAdd        guifg=#40801F ctermfg=28  guibg=#E8EAEF ctermbg=254
hi DiffChange     guifg=#986800 ctermfg=136 guibg=#E8EAEF ctermbg=254
hi DiffDelete     guifg=#D03E3E ctermfg=160 guibg=#E8EAEF ctermbg=254
hi DiffText       guifg=#F4F5F7 ctermfg=255 guibg=#986800 ctermbg=136 gui=bold cterm=bold
