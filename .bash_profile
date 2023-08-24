[[ -f ~/.bashrc ]] && source ~/.bashrc
[[ -z "$DISPLAY" && `tty` == /dev/tty1 ]] && {
	emacs --daemon &
	exec Hyprland
}
