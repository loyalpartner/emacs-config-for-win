;;; init-lispy.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 lee
;;
;; Author: lee <http://github/lee>
;; Maintainer: lee <loyalpartner@163.com>
;; Created: March 05, 2020
;; Modified: March 05, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/lee/init-lispy
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

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
  :hook ((common-lisp-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)
         (racket-mode . lispy-mode)
         (hy-mode . lispy-mode)
         (lfe-mode . lispy-mode)
         (dune-mode . lispy-mode)
         (clojure-mode . lispy-mode))
  :config
  (setq lispy-close-quotes-at-end-p t)
  (define-key lispy-other-mode-map (kbd "t") #'lispy-move-left)
  (add-hook 'lispy-mode-hook #'turn-off-smartparens-mode))

(provide 'init-lispy)
