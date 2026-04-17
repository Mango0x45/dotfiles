let s:cfg = getenv('XDG_CONFIG_HOME')
if s:cfg == v:null
	let s:cfg = '~/.config/aerc/'
else
	let s:cfg .= '/aerc/'
endif

let s:ipaths
	\ = s:cfg . '*.conf,'
	\ . s:cfg . 'stylesets/*,'
	\ . '/usr/share/aerc/*.conf,'
	\ . '/usr/share/aerc/stylesets/*'
let s:gpaths
	\ = s:cfg . 'templates/*,'
	\ . '/usr/share/aerc/templates/*'

execute 'autocmd BufRead,BufNewFile ' . s:ipaths . ' setfiletype ini'
execute 'autocmd BufRead,BufNewFile ' . s:gpaths . ' setfiletype gotmpl'
