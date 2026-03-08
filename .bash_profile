[[ -z "$XDG_RUNTIME_DIR" ]] && export XDG_RUNTIME_DIR="/run/user/$UID"

if [[ ! -d "$XDG_RUNTIME_DIR" ]]
then
	mkdir "$XDG_RUNTIME_DIR"
	chmod 0700 "$XDG_RUNTIME_DIR"
fi

readonly tty="$(tty)"
[[ -f ~/.bashrc ]] && source ~/.bashrc
if [[ -z "$DISPLAY" && -z "$NIRI_LOADED" && "$tty" = /dev/tty1 ]]
then
	export LIBSEAT_BACKEND=seatd
	export NIRI_LOADED=1
	[[ "$(hostname)" = "mangobox" ]] && exec dbus-run-session niri --session
	exec niri-session
fi
