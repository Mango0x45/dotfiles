;;; mm-lsp.el --- Language Server Protocol configuration  -*- lexical-binding: t; -*-

;;; Configure LSP

(defun mm-lsp-eglot-no-inlay-hints ()
  "Disable inlay hints when `eglot' is enabled."
  (eglot-inlay-hints-mode -1))

(use-package eglot
  :hook (((c-mode c-ts-mode
           c++-mode c++-ts-mode
           go-ts-mode
           js-mode js-ts-mode)
          . eglot-ensure)
         (eglot-managed-mode . mm-lsp-eglot-no-inlay-hints))
  :init
  (fset #'jsonrpc--log-event #'ignore)
  :custom
  (eglot-events-buffer 0)
  (eglot-extend-to-xref t)
  :config
  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode)
                 . ("clangd" "--header-insertion=never"))))

(use-package eglot-booster
  :after eglot
  :config
  (eglot-booster-mode))


;;; Use Tempel for Snippets

(defun mm-lsp-eglot-tempel-enable ()
  "Enable `eglot-tempel-mode'.
If `eglot-tempel-mode' is already enabled this function does nothing."
  (unless (default-value eglot-tempel-mode)
    (eglot-tempel-mode)))

(use-package eglot-tempel
  :after eglot
  :hook (eglot-managed-mode . mm-lsp-eglot-tempel-enable))

(provide 'mm-lsp)
