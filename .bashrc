# If not running interactively, don't do anything
[[ $- != *i* ]] && return
[[ -f /etc/bashrc ]] && . /etc/bashrc

function __dir_search {
	local qry="$1"
	local base="$2"
	shift 2

	local dir=$(
		find "$base" "$@" -type d -printf '%P\n' \
		| sed 1i. \
		| sort -r \
		| fzf -q "$qry"
	)
	[[ -n "$dir" ]] && pushd "$base/$dir"
}

function jr {
	__dir_search "$1" "$REPODIR" -mindepth 2 -maxdepth 2
}

function jc {
	__dir_search "$1" "$XDG_CONFIG_HOME" -maxdepth 1 -type d
}

function goto {
	pushd "$(fzf --literal --filepath-word --walker=dir --walker-root="$HOME")"
}

export BROWSER="firefox"
export CC="gcc"
export EDITOR="nvim"
export PAGER="less"
export MANPAGER="nvimpager"
export TERM="xterm-256color"
export VISUAL="nvim"
export MANSECT="3,2,1,8,5,7,4,6,3p,1p,0p"
export LC_NUMERIC="nl_NL.UTF-8"

export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_DIRS="/etc/xdg"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_DIRS="/usr/local/share:/usr/share"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

export XDG_DESKTOP_DIR="$HOME"
export XDG_DOCUMENTS_DIR="$HOME/doc"
export XDG_DOWNLOAD_DIR="$HOME/down"
export XDG_MUSIC_DIR="$HOME/media/mus"
export XDG_PICTURES_DIR="$HOME/media/gfx"
export XDG_PUBLICSHARE_DIR="$HOME"
export XDG_TEMPLATES_DIR="$HOME"
export XDG_VIDEOS_DIR="$HOME/media/vid"

xdg-user-dirs-update --set DESKTOP     "$XDG_DESKTOP_DIR"
xdg-user-dirs-update --set DOCUMENTS   "$XDG_DOCUMENTS_DIR"
xdg-user-dirs-update --set DOWNLOAD    "$XDG_DOWNLOAD_DIR"
xdg-user-dirs-update --set MUSIC       "$XDG_MUSIC_DIR"
xdg-user-dirs-update --set PICTURES    "$XDG_PICTURES_DIR"
xdg-user-dirs-update --set PUBLICSHARE "$XDG_PUBLICSHARE_DIR"
xdg-user-dirs-update --set TEMPLATES   "$XDG_TEMPLATES_DIR"
xdg-user-dirs-update --set VIDEOS      "$XDG_VIDEOS_DIR"

export ASPELL_CONF="per-conf $XDG_CONFIG_HOME/aspell/aspell.conf; personal $XDG_CONFIG_HOME/aspell/en.pws; repl $XDG_CONFIG_HOME/aspell/en.prepl"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export GOPATH="$XDG_DATA_HOME/go"
export GOROOT="/usr/lib/go"
export GRADLE_USER_HOME="$XDG_DATA_HOME/gradle"
export HISTFILE="$XDG_STATE_HOME/bash/history"
export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"
export MAILDIR="$HOME/mail"
export NODE_REPL_HISTORY="$XDG_DATA_HOME/node_repl_history"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export NVM_DIR="$XDG_DATA_HOME/nvm"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/pass"
export PYTHONCACHEPREFIX="$XDG_CACHE_HOME/python"
export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/startup.py"
export PYTHONUSERBASE="$XDG_DATA_HOME/python"
export RLWRAP_HOME="$XDG_CACHE_HOME/rlwrap"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
export SQLITE_HISTORY="$XDG_STATE_HOME/sqlite_history"
export W3M_DIR="$XDG_STATE_HOME/w3m"
export WGETRC="$XDG_CONFIG_HOME/wgetrc"
export XCOMPOSECACHE="$XDG_CACHE_HOME/X11/xcompose"
export XCOMPOSEFILE="$XDG_CONFIG_HOME/X11/xcompose"
export _JAVA_OPTIONS="-Djava.util.prefs.userRoot=\"$XDG_CONFIG_HOME/java\" -Djavafx.cachedir=\"$XDG_CACHE_HOME/openjfx\""

export REPODIR="$HOME/code"
export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
export PATH="$HOME/.local/bin:$HOME/.local/sbin:$XDG_DATA_HOME/qlot/bin:/opt/odin:$GOROOT/bin:$GOPATH/bin:$XDG_DATA_HOME/npm/bin:$PATH"

export CLANGD_FLAGS='-header-insertion=never'
export GPG_TTY=`tty`
export GTK_IM_MODULE=xim
export HISTSIZE=100000
export LESS='-RF'
export PRINTER='HP_OfficeJet_8010'
export QT_IM_MODULE=xim
export VAULT_2FA='2 Factor Authentication'

alias irssi='irssi --config="$XDG_CONFIG_HOME/irssi/config" --home="$XDG_DATA_HOME/irssi"'
alias ..='cd ..'
alias d='git --git-dir="$REPODIR/Mango0x45/dotfiles.git" --work-tree="$HOME"'
alias g=git
alias grep='grep --color=auto'
alias la='ls --color=auto -Av    --group-directories-first'
alias ll='ls --color=auto -AGhlv --group-directories-first --time-style="+%d %b %Y %T"'
alias ls='ls --color=auto -v     --group-directories-first'
alias sl='sl -ac5'
alias sv=sudoedit
alias v="$VISUAL"
alias z='2>/dev/null zathura --fork "$@" 2>/dev/null'

PS1='\[\e[96;1m\]\u \[\e[39m\]\W \[\e[96m\]〉\[\e[0m\]'

eval "$(fzf --bash)"

# Autocompletions for auto-cpufreq.  Inlined from the generated output
# for performance reasons.
_auto_cpufreq_completion() {
	local IFS=$'\n'
	local response

	response=$(
		env \
			COMP_WORDS="${COMP_WORDS[*]}"        \
			COMP_CWORD=$COMP_CWORD               \
			_AUTO_CPUFREQ_COMPLETE=bash_complete \
			$1
	)

	for completion in $response; do
		IFS=',' read type value <<<"$completion"

		case $type in
		dir)
			COMPREPLY=()
			compopt -o dirnames
			;;
		file)
			COMPREPLY=()
			compopt -o default
			;;
		plain)
			COMPREPLY+=($value)
			;;
		esac
	done

	return 0
}

complete -o nosort -F _auto_cpufreq_completion auto-cpufreq

# sed '1s/^/[3m‘/; $s/$/’[0m/' "$XDG_CACHE_HOME/qotd" | fold -sw 80
