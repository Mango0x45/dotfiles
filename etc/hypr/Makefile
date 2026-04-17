all: hyprland.conf

hyprland.conf: macros.m4 hyprland.conf.in
	m4 macros.m4 hyprland.conf.in >$@
