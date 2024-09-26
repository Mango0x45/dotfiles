# command -v emacs >/dev/null && emacs --daemon --debug-init
[[ -f ~/.bashrc ]] && source ~/.bashrc
[[ -z "$DISPLAY" && `tty` == /dev/tty1 ]] && exec Hyprland
