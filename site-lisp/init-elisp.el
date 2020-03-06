;;; init-elisp.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 lee
;;
;; Author: lee <http://github/lee>
;; Maintainer: lee <loyalpartner@163.com>
;; Created: March 05, 2020
;; Modified: March 05, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/lee/init-elisp
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(use-package elisp-mode
  :config
  (emacs-lisp-mode))

(use-package helpful
  :straight t
  :commands
  helpful-key
  helpful-callable
  helpful-variable
  :config
  (general-def help-map
    "f" #'helpful-callable
    "v" #'helpful-variable
    "k" #'helpful-key))

(use-package lispyville
  :straight t
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '((operators normal)
     c-w
     (prettify insert)
     (atom-movement normal visual)
     slurp/barf-lispy
     (wrap normal insert)
     additional
     additional-insert
     (additional-wrap normal insert)
     (escape insert))))

(use-package lispy
  :straight t
  :hook (emacs-lisp-mode . lispy-mode))

(provide 'init-elisp)
;;; init-elisp.el ends here
