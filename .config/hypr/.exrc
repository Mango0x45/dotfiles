autocmd BufWritePost hyprland.conf.in make
autocmd BufRead hyprland.conf.in setlocal commentstring=#\ %s filetype=hyprlang
