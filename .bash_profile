readonly tty="$(tty)"
[[ -f ~/.bashrc ]] && source ~/.bashrc
if [[ -z "$DISPLAY" && -z "$NIRI_LOADED" && "$tty" = /dev/tty1 ]]
then
	export NIRI_LOADED=1
	exec niri-session
fi
[[ -z "$DISPLAY" && "$tty" = /dev/tty2 ]] && exec start-hyprland
