;; Disable useless visual elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; So we can let the WM make this a floating window
(setq frame-title-format "ec")

;; XDG Directories
(defconst xdg-cache-home
  (or (getenv "XDG_CACHE_HOME")
      (expand-file-name ".cache" (getenv "HOME"))))
(defconst xdg-config-home
  (or (getenv "XDG_CONFIG_HOME")
      (expand-file-name ".config" (getenv "HOME"))))

;; Keep the config directory clean
(let ((cache-dir (expand-file-name "ec" xdg-cache-home)))
  (setq user-emacs-directory cache-dir
        auto-save-list-file-prefix (expand-file-name
                                    "auto-save-list/"
                                    cache-dir)
        backup-directory-alist `(("." . ,(expand-file-name
                                          "backups"
                                          cache-dir)))))

;; Themeing
(add-to-list 'custom-theme-load-path (expand-file-name "emacs" xdg-config-home))
(add-to-list 'default-frame-alist '(alpha-background . 90))
(add-hook 'after-make-frame-functions
          (lambda (_)
            (dolist (face '(default fixed-pitch))
              (set-face-attribute face nil
                                  :font "Iosevka Smooth"
                                  :weight 'regular
                                  :height 162))))
(load-theme 'mango t)

;; Set default settings
(with-eval-after-load 'calc
  (setopt calc-show-banner nil
          initial-buffer-choice (lambda () (get-buffer "*Calculator*")))
  (keymap-set calc-mode-map "q" #'delete-frame))

;; Enter ‘full-calc’ mode
(full-calc)
