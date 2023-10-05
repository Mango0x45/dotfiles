#set -o vi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
[[ -f /etc/bashrc ]] && . /etc/bashrc

jr() {
	local dir
	dir=`find "$REPODIR" -maxdepth 2 -path "$REPODIR/*/*" -printf '%P\n'    \
		| sort -r                                                       \
		| fzf -q "$1"`
	[ -n "$dir" ] && cd "$REPODIR/$dir"
}

export BROWSER="firefox"
export CC="cc"
export EDITOR="nvim"
export VISUAL="nvim"
export TERM="xterm-256color"

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

export CARGO_HOME="$XDG_DATA_HOME/cargo"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export GOPATH="$XDG_DATA_HOME/go"
export GOROOT="/usr/lib/go"
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
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
export WGETRC="$XDG_CONFIG_HOME/wgetrc"
export _JAVA_OPTIONS="-Djava.util.prefs.userRoot=\"$XDG_CONFIG_HOME/java\""

export REPODIR="$HOME/code/repo"
export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
export PATH="$HOME/.local/bin:$HOME/.local/sbin:$GOROOT/bin:$GOPATH/bin:$PATH"

export GPG_TTY=`tty`
export VAULT_2FA='2 Factor Authentication'

alias ..='cd ..'
alias d='git --git-dir="$REPODIR/Mango0x45/dotfiles.git" --work-tree="$HOME"'
alias g=git
alias grep='grep --color=auto'
alias la='ls --color=auto -A'
alias ll='ls --color=auto -Alh'
alias ls='ls --color=auto'
alias sl='sl -ac5'
alias sv=sudoedit
alias v="$VISUAL"
alias nv='neovide --nofork'

PS1='\[\e[96;1m\]\u \[\e[39m\]\W \[\e[96m\]〉\[\e[0m\]'

sed '1s/^/[3m‘/; $s/$/’[0m/' "$XDG_CACHE_HOME/qotd" | fold -sw 80
