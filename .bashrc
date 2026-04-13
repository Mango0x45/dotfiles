# If not running interactively, don’t do anything
[[ $- != *i* ]] && return

for f in /etc/bashrc /etc/bash/bashrc ~/.config/setup-env
do
	[ -f $f ] && . $f
done

shopt -s histappend
export HISTCONTROL=ignoredups:erasedups

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
	[ -n "$dir" ] && pushd "$base/$dir"
}

function jr {
	__dir_search "$1" "$REPODIR" -mindepth 2 -maxdepth 2
}

function jc {
	__dir_search "$1" "$XDG_CONFIG_HOME" -maxdepth 1 -type d
}

function goto {
	local dst="$(fzf --literal --filepath-word --walker=dir --walker-root="$HOME")" \
	&& pushd "$dst"
}

alias irssi='irssi --config="$XDG_CONFIG_HOME/irssi/config" --home="$XDG_DATA_HOME/irssi"'
alias ..='cd ..'
alias d='git --git-dir="$REPODIR/@me/dotfiles.git" --work-tree="$HOME"'
alias g=git
alias grep='grep --color=auto'
alias la='ls --color=auto -xvA    --group-directories-first'
alias ll='ls --color=auto -xvAGhl --group-directories-first --time-style="+%d %b %Y"'
alias ls='ls --color=auto -xv     --group-directories-first'
alias sl='sl -ac5'
alias de=doasedit
alias se=sudoedit
alias e="$VISUAL"
alias z='2>/dev/null zathura --fork "$@"'

__ps1_newline() {
	local _ y x _
	local RESET="\e[0m"
	local HL="\e[30;47m"

	IFS='[;' read -p $'\e[6n' -d R -rs _ y x _
	[[ "$x" != 1 ]] && printf "${HL}␀\n${RESET}"
}

PS1="\$(__ps1_newline)"'\[\e[96;1m\]\u \[\e[39m\]\W \[\e[96m\]〉\[\e[0m\]'

command -v fzf >/dev/null && eval "$(fzf --bash)"
command -v niri >/dev/null && eval "$(niri completions bash)"

if [ -d "$NVM_DIR" ]
then
	. "$NVM_DIR/nvm.sh"
	. "$NVM_DIR/bash_completion"
fi

stty -ixon -ixoff
