[[ -f ~/.bashrc ]] && source ~/.bashrc
[[ -z "$DISPLAY" && `tty` == /dev/tty1 ]] && exec start-hyprland
