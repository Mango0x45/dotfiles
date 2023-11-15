syn match yaccDefines '^%\(header\|output\)\>'
syn match yaccKey '^\s*%\(nterm\|precedence\|nterm\)\>' contained
syn match yaccType '<[a-zA-Z_][a-zA-Z0-9_]*\s*\*>' contains=yaccBrkt contained
syn match yaccString '"[^"]*"' contained
syn match yaccVar '@\(\$\|\d\+\)' containedin=cParen,cPreProc,cMulti contained
