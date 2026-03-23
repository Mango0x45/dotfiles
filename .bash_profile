[ -z "$XDG_RUNTIME_DIR" ] && export XDG_RUNTIME_DIR="/run/user/$UID"

if [ ! -d "$XDG_RUNTIME_DIR" ]
then
	mkdir "$XDG_RUNTIME_DIR"
	chmod 0700 "$XDG_RUNTIME_DIR"
fi

readonly tty="$(tty)"
[ -f ~/.bashrc ] && source ~/.bashrc
