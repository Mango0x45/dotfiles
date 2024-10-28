;;; combobulate-c.el --- C support for combobulate  -*- lexical-binding: t; -*-

(require 'combobulate-manipulation)
(require 'combobulate-navigation)
(require 'combobulate-rules)
(require 'combobulate-settings)
(require 'combobulate-setup)

(eval-and-compile
  (defvar combobulate-c-definitions
    '((context-nodes
       '("char_literal" "false" "field_identifier" "identifier" "null"
         "number_literal" "statement_identifier" "string_literal" "true"
         "type_identifier")))))

(define-combobulate-language
 :name c
 :language c
 :major-modes (c-ts-mode)
 :custom combobulate-c-definitions
 :setup-fn combobulate-c-setup)

(defun combobulate-c-setup (_))

(provide 'combobulate-c)
