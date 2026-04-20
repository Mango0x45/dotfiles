set background=dark
highlight clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "mango-dark"

" Normal & Background
hi Normal         guifg=#D1D5D8 ctermfg=251 guibg=#2B303B ctermbg=236
hi EndOfBuffer    guifg=#1D232F ctermfg=234 guibg=#2B303B ctermbg=236
hi SignColumn     guibg=#2B303B ctermbg=236

" Cursor & Lines
hi Cursor         guifg=#2B303B ctermfg=236 guibg=#D1D5D8 ctermbg=251
hi CursorLine     guibg=#414859 ctermbg=238 term=NONE cterm=NONE gui=NONE
hi ColorColumn    guibg=#414859 ctermbg=238
hi LineNr         guifg=#939CA8 ctermfg=246 guibg=#2B303B ctermbg=236
hi CursorLineNr   guifg=#ECA671 ctermfg=216 guibg=#2B303B ctermbg=236 term=bold cterm=bold gui=bold

" Selection
hi Visual         guibg=#414859 ctermbg=238

" Window & Status
hi VertSplit      guifg=#1D232F ctermfg=234 guibg=#1D232F ctermbg=234
hi StatusLine     guifg=#D1D5D8 ctermfg=251 guibg=#414859 ctermbg=238 gui=bold cterm=bold
hi StatusLineNC   guifg=#939CA8 ctermfg=246 guibg=#1D232F ctermbg=234 gui=NONE cterm=NONE
hi TabLine        guifg=#939CA8 ctermfg=246 guibg=#1D232F ctermbg=234 gui=NONE cterm=NONE
hi TabLineFill    guibg=#1D232F ctermbg=234
hi TabLineSel     guifg=#D1D5D8 ctermfg=251 guibg=#2B303B ctermbg=236 gui=bold cterm=bold

" Syntax
hi Comment        guifg=#939CA8 ctermfg=246
hi Constant       guifg=#ECA671 ctermfg=216
hi String         guifg=#ECA671 ctermfg=216
hi Number         guifg=#ECA671 ctermfg=216
hi Identifier     guifg=#7DC1E6 ctermfg=117 gui=NONE cterm=NONE
hi Function       guifg=#E5D070 ctermfg=228
hi Statement      guifg=#C678DD ctermfg=176 gui=NONE cterm=NONE
hi Keyword        guifg=#C678DD ctermfg=176 gui=NONE cterm=NONE
hi PreProc        guifg=#FFACFF ctermfg=219 gui=bold cterm=bold
hi Type           guifg=#569CD6 ctermfg=74  gui=NONE cterm=NONE
hi Special        guifg=#F24E4E ctermfg=160
hi Error          guifg=#F24E4E ctermfg=160 guibg=NONE  gui=bold,reverse cterm=bold,reverse
hi Todo           guifg=#E183E8 ctermfg=213 guibg=NONE  gui=bold cterm=bold
hi MatchParen     guifg=#C4F553 ctermfg=191 guibg=#414859 ctermbg=238 gui=bold cterm=bold

" Diffs
hi DiffAdd        guifg=#A6E22E ctermfg=156 guibg=#1D232F ctermbg=234
hi DiffChange     guifg=#E5D070 ctermfg=228 guibg=#1D232F ctermbg=234
hi DiffDelete     guifg=#F24E4E ctermfg=160 guibg=#1D232F ctermbg=234
hi DiffText       guifg=#2B303B ctermfg=236 guibg=#E5D070 ctermbg=228 gui=bold cterm=bold
